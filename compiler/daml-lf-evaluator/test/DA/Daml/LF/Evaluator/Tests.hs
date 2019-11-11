-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module DA.Daml.LF.Evaluator.Tests
  ( main
  ) where

import DA.Bazel.Runfiles (locateRunfiles,mainWorkspace)
import DA.Daml.LF.Reader (readDalfs,Dalfs(..))
import Data.Int (Int64)
import System.Environment.Blank (setEnv)
import System.FilePath ((</>))
import qualified "zip-archive" Codec.Archive.Zip as ZipArchive
import qualified DA.Daml.LF.Ast as LF
import qualified DA.Daml.LF.Evaluator as EV
import qualified Data.ByteString as BS (readFile)
import qualified Data.ByteString.Lazy as BSL (fromStrict)
import qualified Data.Text as Text
import qualified Test.Tasty as Tasty (defaultMain,testGroup,TestTree)
import qualified Test.Tasty.HUnit as Tasty (assertBool,assertEqual,testCase)

main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" "1" True
  run tests

tests :: [Test]
tests =
  [ Test "fact" 4 24
  , Test "fact" 5 120

  , Test "dub_dub_dub" 1 8

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

    -- check the original program evaluates as expected
    let prog = EV.simplify ddar mn vn
    let (actual,countsOrig) = EV.runIntProgArg prog arg
    Tasty.assertEqual "original" expected actual

    -- check the normalized program evaluates as expected
    progN <- EV.normalize EV.defaultConfig prog
    let (actualN,countsNorm) = EV.runIntProgArg progN arg
    Tasty.assertEqual "normalized" expected actualN

    -- check the normalized program didn't take more steps to evaluate
    -- for each of the 3 classes of step
    -- and in the case of apps it actually reduced (showing normalization did something)
    let EV.Counts{apps=a1,prims=p1,projections=q1} = countsOrig
    let EV.Counts{apps=a2,prims=p2,projections=q2} = countsNorm
    let mkName tag x y = tag <> ":" <> show x <> "-->" <> show y
    Tasty.assertBool (mkName "apps" a1 a2) (a2 < a1)
    Tasty.assertBool (mkName "prim" p1 p2) (p2 <= p1)
    Tasty.assertBool (mkName "proj" q1 q2) (q2 <= q1)
