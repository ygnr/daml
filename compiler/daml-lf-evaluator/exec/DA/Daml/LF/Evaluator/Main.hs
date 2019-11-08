-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module DA.Daml.LF.Evaluator.Main
  ( main
  ) where

import Control.Monad (forM_)
import DA.Bazel.Runfiles (locateRunfiles,mainWorkspace)
import DA.Daml.LF.Evaluator.Pretty (ppExp)
import DA.Daml.LF.Evaluator.Simp (simplify)
import DA.Daml.LF.Evaluator.Norm (normalize)
import DA.Daml.LF.Reader (readDalfs,Dalfs(..))
import Data.Int (Int64)
import System.FilePath ((</>), isExtensionOf)
import qualified "zip-archive" Codec.Archive.Zip as ZipArchive
import qualified DA.Daml.LF.Ast as LF
import qualified DA.Daml.LF.Evaluator as EV
import qualified DA.Daml.LF.Evaluator.Exp as Exp
import qualified Data.ByteString as BS (readFile)
import qualified Data.ByteString.Lazy as BSL(fromStrict)
import qualified Data.Text as Text

example :: (String,Int64)
--example = ("dub_dub_dub",1)
--example = ("sub",0)
--example = ("thrice_sub",0)
--example = ("thrice_thrice_sub",0)
--example = ("fact",5)
--example = ("nthPrime",100)
example = ("run_makeDecimal",7)

main :: IO ()
main = do
  filename <- locateRunfiles (mainWorkspace </> "compiler/daml-lf-evaluator/examples.dar")
  dalfs <- readDar filename
  ddar <- EV.decodeDalfs dalfs
  let (functionName,arg) = example
  let mn = LF.ModuleName ["Examples"]
  let vn = LF.ExprValName $ Text.pack functionName
  let prog = simplify ddar mn vn
  let title = functionName
  putStrLn $ "arg = " <> show arg

  showProg title prog
  showEvaluation "evaluation,original" prog arg

  putStrLn $ "normalizing..."

  progN <- normalize prog
  showProg "normalized" progN
  showEvaluation "evaluation,normalized" progN arg


showEvaluation :: String -> Exp.Prog -> Int64 -> IO ()
showEvaluation title prog arg = do
  putStrLn $ "--["<>title<>"]----------------------------"
  let (res,count) = EV.runIntProgArg prog arg
  putStrLn $ "result = " <> show res <> ", #apps = " <> show count


showProg :: String -> Exp.Prog -> IO ()
showProg title prog = do
  let Exp.Prog{defs,main} = prog
  putStrLn $ "==[" <> title <> "]================================================"
  putStrLn $ ppExp main
  putStrLn "--[defs]----------------------------"
  forM_ (zip [0::Integer ..] defs) $ \(i,(Exp.DefKey(_,_,name),exp)) ->
    putStrLn $ show i <> "("<> Text.unpack (LF.unExprValName name) <> "): " <> ppExp exp

readDar :: FilePath -> IO Dalfs
readDar inFile = do
  if "dar" `isExtensionOf` inFile then return () else fail "must be a dar"
  archiveBS <- BS.readFile inFile
  either fail pure $ readDalfs $ ZipArchive.toArchive $ BSL.fromStrict archiveBS
