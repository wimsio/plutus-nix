module Main (main) where

import Test.Tasty              (defaultMain, testGroup)
import qualified VestingSpec               as VS
import qualified ParameterizedVestingSpec  as PVS
import qualified CGTimeSpec                as TS
import qualified CGPlutusUtilsSpec         as US

main :: IO ()
main = defaultMain $
  testGroup "All wspace Tests"
    [ VS.tests
    , PVS.tests
    , TS.tests
    , US.tests
    ]

