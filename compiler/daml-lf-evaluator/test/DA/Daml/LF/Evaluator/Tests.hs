-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module DA.Daml.LF.Evaluator.Tests
  ( main
  ) where

import DA.Bazel.Runfiles (locateRunfiles,mainWorkspace)
import DA.Daml.LF.Reader (readDalfs,Dalfs(..))
import Data.Int (Int64)
import System.FilePath ((</>))
import qualified "zip-archive" Codec.Archive.Zip as ZipArchive
import qualified DA.Daml.LF.Ast as LF
import qualified DA.Daml.LF.Evaluator as EV
import qualified Data.ByteString as BS (readFile)
import qualified Data.ByteString.Lazy as BSL (fromStrict)
import qualified Data.Text as Text
import qualified Test.Tasty as Tasty (defaultMain,testGroup,TestTree)
import qualified Test.Tasty.HUnit as Tasty (assertEqual,testCase)

main :: IO ()
main = run tests

tests :: [Test]
tests =
  [ Test "fact" 4 24
  , Test "fact" 5 120

  , Test "sub" 0 (-1)
  , Test "thrice_sub" 0 (-3)
  , Test "thrice_thrice_sub" 0 (-27)

  , Test "length_list" 7 3
  , Test "sum_list" 7 24
  , Test "run_makeDecimal" 7 789

  , Test "nthPrime" 10 29
  , Test "nthPrime" 100 541

  , Test "run_sum_myList" 9 30
  , Test "run_sum_myList2" 99 300

  ]

-- testing for DAML functions of type: `Int -> Int`
data Test = Test
  { functionName :: String
  , arg :: Int64
  , expected :: Int64
  }

run :: [Test] -> IO ()
run tests = do
  filename <- locateRunfiles (mainWorkspace </> "compiler/daml-lf-evaluator/examples.dar")
  dalfs <- readDar filename
  ddar <- EV.decodeDalfs dalfs
  Tasty.defaultMain $ Tasty.testGroup "daml-lf-evaluator" (map (makeTasty ddar) tests)

readDar :: FilePath -> IO Dalfs
readDar inFile = do
  archiveBS <- BS.readFile inFile
  either fail pure $ readDalfs $ ZipArchive.toArchive $ BSL.fromStrict archiveBS

makeTasty :: EV.DecodedDar -> Test -> Tasty.TestTree
makeTasty ddar Test{functionName,arg,expected} = do
  let mn = LF.ModuleName ["Examples"]
  let vn = LF.ExprValName $ Text.pack functionName
  let name = Text.unpack (LF.unExprValName vn) <> "(" <> show arg <> ")"
  Tasty.testCase name $ do
    let prog = EV.simplify ddar mn vn
    let (actual,_count) = EV.runIntProgArg prog arg
    Tasty.assertEqual "" expected actual
    -- TODO: test normalization for all examples
