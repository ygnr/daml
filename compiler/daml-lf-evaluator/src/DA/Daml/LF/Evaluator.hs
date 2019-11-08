-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}

module DA.Daml.LF.Evaluator
  ( DecodedDar(..),
    decodeDalfs,
    simplify,
    normalize,
    runIntProgArg,
  ) where

import DA.Daml.LF.Evaluator.Eval (runIntProgArg)
import DA.Daml.LF.Evaluator.Simp (DecodedDar(..),simplify)
import DA.Daml.LF.Evaluator.Norm (normalize)
import DA.Daml.LF.Proto3.Archive (decodeArchive)
import DA.Daml.LF.Reader (Dalfs(..))
import qualified DA.Daml.LF.Ast as LF
import qualified Data.ByteString.Lazy as BSL (ByteString,toStrict)
import qualified Data.Map.Strict as Map

decodeDalfs :: Dalfs -> IO DecodedDar
decodeDalfs Dalfs{mainDalf,dalfs} = do
  (mainId,mainPackage) <- decodeDalf mainDalf
  otherIdentifiedPackages <- mapM decodeDalf dalfs
  let packageMap = Map.fromList $ otherIdentifiedPackages <> [(mainId,mainPackage)]
  return $ DecodedDar { mainId, packageMap }
  where
    decodeDalf :: BSL.ByteString -> IO (LF.PackageId,LF.Package)
    decodeDalf dalfBS = do
      Right pair <- return $ decodeArchive $ BSL.toStrict dalfBS
      return pair
