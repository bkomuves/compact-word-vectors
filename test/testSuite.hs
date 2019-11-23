
-- | The test-suite

module Main where

--------------------------------------------------------------------------------

import Test.Tasty

import qualified Tests.Blob
import qualified Tests.WordVec
import qualified Tests.IntVec

-- import System.Random

--------------------------------------------------------------------------------

main = do
  -- setStdGen (mkStdGen 12345)        -- only for reproducible timing!
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests"  
  [ Tests.Blob.all_tests
  , Tests.WordVec.all_tests
  , Tests.IntVec.all_tests
  ]

--------------------------------------------------------------------------------

