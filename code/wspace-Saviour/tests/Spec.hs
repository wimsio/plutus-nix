{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
 
import Test.Tasty (defaultMain, testGroup, TestTree)

import CGPlutusUtilsSpec       qualified as CGPlutusUtilsSpec
import CGTimeSpec              qualified as CGTimeSpec
import ParameterizedVestingSpec qualified as ParameterizedVestingSpec
import VestingSpec             qualified as VestingSpec
import DemoSpec                qualified as DemoSpec


main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests =
  testGroup "All wspace Tests"
    [ VestingSpec.tests
    , ParameterizedVestingSpec.tests
    , CGTimeSpec.tests
    , CGPlutusUtilsSpec.tests
    , DemoSpec.tests          
    ]
