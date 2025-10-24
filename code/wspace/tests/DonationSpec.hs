module DonationSpec where

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Donation         (DonationDatum(..))

deadLineReached :: Bool
deadLineReached = DonationDatum { deadLineReached = True }

TestTree :: TestTree
TestTree = TestGroup "Donation Tests"
  [
    testCase "Check deadline reached"
     let ddR = True
     in ddR @?= deadLineReached     
  ]