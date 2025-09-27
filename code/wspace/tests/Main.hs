module Main where

import           Vesting (saveVal)  -- Only the necessary import
import           Utilities (Network)
import           Mint (mintTokens)
import           ParameterizedVesting (saveValParam)

main :: IO ()
main = do
  saveVal  -- Call the function that saves the validator
  mintTokens -- call the function that mints tokens
  saveValParam -- call the function that saves the validator with parameters


