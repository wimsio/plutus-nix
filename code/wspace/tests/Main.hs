module Main where

import           Vesting (saveVal)  -- Only the necessary import
import           Utilities (Network)

main :: IO ()
main = do
  saveVal  -- Call the function that saves the validator

