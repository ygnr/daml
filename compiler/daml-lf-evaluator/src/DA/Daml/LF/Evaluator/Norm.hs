-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}

module DA.Daml.LF.Evaluator.Norm
  ( normalize,
  ) where

import Control.Monad (forM,liftM,ap)
import DA.Daml.LF.Evaluator.Exp (Prog(..),Exp,Alt,Var,FieldName)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified DA.Daml.LF.Ast as LF
import qualified DA.Daml.LF.Evaluator.Exp as Exp
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text


-- entry point -- IO in return type only for dev-time debug
normalize :: Prog -> IO Prog
normalize Prog{defs,main} =
  run defs $ do
    --IO $ putStrLn "norm:main.."
    main <- norm main >>= reify
    defs <- forM defs $ \(name,exp) -> do
      --IO $ print ("norm"::String,name)
      exp <- norm exp >>= reify
      return (name,exp)
    return $ Prog{defs,main}

norm :: Exp -> Effect Norm
norm = \case
  e@(Exp.Lit _v) -> return $ Syntax e

  Exp.Var x -> do
    env <- GetEnv
    case Map.lookup x env of
      Nothing -> error $ "norm, " <> show x
      Just v -> return v

  Exp.App e1 e2 -> do
    v1 <- norm e1
    v2 <- norm e2
    apply (v1,v2)

  Exp.Lam x body -> do
    restore <- Save
    return $ Macro $ \arg ->
      restore $ ModEnv (Map.insert x arg) $ norm body

  Exp.Let x rhs body -> do
    v <- norm rhs
    ModEnv (Map.insert x v) $ norm body

  Exp.Rec elems -> do
    xs <- forM elems $ \(name,exp) -> do
      n <- norm exp
      return (name,n)
    return $ Record xs

  Exp.Dot exp name -> do
    r <- norm exp
    normProjectRec name r

  Exp.Con tag elems -> do
    elems <- forM elems $ \elem -> do
      norm elem >>= reify
    --return $ Value.Constructed tag vs
    return $ Syntax $ Exp.Con tag elems

  Exp.Match{scrut,alts} -> do
    scrut <- norm scrut >>= reify
    alts <- mapM normAlt alts
    return $ Syntax $ Exp.Match {scrut,alts}

  e@(Exp.Ref i) -> do
    MaybeInline i $ \case
      Nothing -> return $ Syntax e
      Just exp -> norm exp


normAlt :: Alt -> Effect Alt
normAlt = \case
  Exp.Alt{tag,bound,rhs} -> do
    -- TODO: think need to generate new vars here !
    rhs <- underBoundVars bound $ norm rhs >>= reify
    return $ Exp.Alt{tag,bound,rhs}

underBoundVars :: [Var] -> Effect a -> Effect a
underBoundVars xs e = do
  let f env = foldr (\x -> Map.insert x (Syntax $ Exp.Var x)) env xs
  ModEnv f e


data Norm -- Normalized Expression
  = Syntax Exp
  | Record [(FieldName,Norm)]
  | Macro (Norm -> Effect Norm)

apply :: (Norm,Norm) -> Effect Norm
apply = \case
  (Syntax func, arg) -> do
    exp <- reify arg
    return $ Syntax (Exp.App func exp)
  (Record _, _) -> error "Norm,apply,record"
  (Macro func, arg@(Syntax exp)) ->
    if isAtomic exp
    then func arg
    else do
      x <- Fresh
      body <- func (Syntax (Exp.Var x)) >>= reify
      return $ Syntax $ Exp.Let x exp body
  (Macro func, arg) ->
    func arg

isAtomic :: Exp -> Bool
isAtomic = \case
  Exp.Lit{} -> True
  Exp.Var{} -> True
  _ -> False

reify :: Norm -> Effect Exp
reify = \case
  Syntax exp -> return exp
  Record xs -> do
    elems <- forM xs $ \(name,n) -> do
      exp <- reify n
      return (name,exp)
    return $ Exp.Rec elems
  Macro f -> do
    x <- Fresh
    body <- f (Syntax (Exp.Var x)) >>= reify
    return $ Exp.Lam x body

normProjectRec :: FieldName -> Norm -> Effect Norm
normProjectRec fieldName = \case
  Macro _ -> error "normProjectRec,Macro"
  Syntax exp -> return $ Syntax $ Exp.Dot exp fieldName
  Record xs -> do
    case lookup fieldName xs of
      Nothing -> error $ "normProjectRec, " <> show fieldName
      Just v -> return v


data Effect a where
  Ret :: a -> Effect a
  Bind :: Effect a -> (a -> Effect b) -> Effect b
  GetEnv :: Effect Env
  ModEnv :: (Env -> Env) -> Effect a -> Effect a
  MaybeInline :: Int -> (Maybe Exp -> Effect a) -> Effect a
  Fresh :: Effect Exp.Var
  Save :: Effect (Effect a -> Effect a)
  Restore :: InlineScope -> Env -> Effect a -> Effect a
  IO :: IO a -> Effect a

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind


run :: Exp.Defs -> Effect a -> IO a
run defs e = fst <$> run Set.empty env0 state0 e
  where
    env0 = Map.empty
    state0 = State { unique = 0 }

    run :: InlineScope -> Env -> State -> Effect a -> IO (a,State)
    run scope env state = \case
      IO io -> do x <- io; return (x,state)
      Ret x -> return (x,state)
      Bind e f -> do (v,state') <- run scope env state e; run scope env state' (f v)
      GetEnv -> return (env,state)
      ModEnv f e -> run scope (f env) state e
      Save -> return (Restore scope env, state)
      Restore scope env e -> run scope env state e
      MaybeInline i k -> do
        let doing = Set.member i scope
        let exp = getDef i
        let inlineAnyway = isRecord exp
        let cut = doing && not inlineAnyway
        -- putStrLn $ "Inline, " <> show i <> ", " <> show scope <> (if cut then " -CUT" else "")
        if cut then run scope env state (k Nothing) else do
          run (Set.insert i scope) env state (k (Just exp))

      Fresh -> do
        let State{unique} = state
        let state' = state { unique = unique + 1 }
        let x = LF.ExprVarName $ Text.pack ("_u"<> show unique) -- assumes no clash
        return (x,state')

    getDef :: Int -> Exp
    getDef i =
      case Map.lookup i defs of
        Nothing -> error $ "getDef, " <> show i
        Just (_,exp) -> exp


isRecord :: Exp -> Bool
isRecord = \case
  Exp.Rec{} -> True
  _ -> False

type Env = Map Exp.Var Norm
data State = State { unique :: Unique }
type Unique = Int
type InlineScope = Set Int
