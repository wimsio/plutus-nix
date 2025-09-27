{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Main where

import           Vesting (saveVal)  -- Only the necessary import
import           Utilities (Network)
import           Mint (mintTokens)
import           ParameterizedVesting (VestingParams(..), saveValParam, fromHexPKH)
import Prelude (IO, putStrLn)

-- use the version from your module
import ParameterizedVesting (VestingParams(..), saveValParam, fromHexPKH)

-- you only need POSIXTime ctor here
import Plutus.V2.Ledger.Api (POSIXTime(..))

main :: IO ()
main = do
  saveVal      -- Call the function that saves the validator
  mintTokens   -- Call the function that mints tokens

  let vp = VestingParams
              { beneficiary = fromHexPKH "be1577292869f96572abd53159d6de420ff154a1330636d4ab8f5776"
              , deadline    = POSIXTime 1758984321
              }

  saveValParam vp


