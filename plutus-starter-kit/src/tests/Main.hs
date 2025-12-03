module Main where

import Test.Tasty
import qualified ValidatorLogicSpec        as ValidatorLogicSpec
import qualified ValidatorLogicFormalSpec  as ValidatorLogicFormalSpec

main :: IO ()
main =
  defaultMain $
    testGroup "All tests"
      [ ValidatorLogicSpec.tests
      , ValidatorLogicFormalSpec.tests
      ]

