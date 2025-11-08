{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module ParameterizedVesting where

import           Plutus.V1.Ledger.Interval   (contains)
import           Plutus.V2.Ledger.Api        (BuiltinData, POSIXTime, PubKeyHash(..),
                                              ScriptContext (scriptContextTxInfo),
                                              TxInfo (txInfoValidRange),
                                              Validator, from, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts   (txSignedBy)
import           PlutusTx                    (applyCode, compile, liftCode, makeLift)
import           PlutusTx.Prelude            (Bool(..), traceIfFalse, ($), (&&), (.))
import           Prelude                     (String, IO, Either(..), (++), error)
import           Utilities                   (wrapValidator, writeValidatorToFile)

import qualified PlutusTx.Builtins.Class     as Builtins
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Base16      as B16

--------------------------------------------------------------------------------
-- ON-CHAIN / VALIDATOR
--------------------------------------------------------------------------------

data VestingParams = VestingParams
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    }
makeLift ''VestingParams

{-# INLINABLE mkParameterizedVestingValidator #-}
mkParameterizedVestingValidator :: VestingParams -> () -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator params () () ctx =
    traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
    traceIfFalse "deadline not reached"           deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info (beneficiary params)

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline params) (txInfoValidRange info)

{-# INLINABLE mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator
  :: VestingParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator =
  wrapValidator . mkParameterizedVestingValidator

validator :: VestingParams -> Validator
validator params =
  mkValidatorScript
    ( $$(compile [|| mkWrappedParameterizedVestingValidator ||])
      `applyCode` liftCode params
    )

--------------------------------------------------------------------------------
-- HELPERS
--------------------------------------------------------------------------------

fromHexPKH :: String -> PubKeyHash
fromHexPKH hex =
  case B16.decode (C.pack hex) of
    Right decoded -> PubKeyHash (Builtins.toBuiltin decoded)
    Left err      -> error ("Hex decoding failed: " ++ err)

saveValParam :: VestingParams -> IO ()
saveValParam = writeValidatorToFile "./assets/parameterized-vesting.plutus" . validator
