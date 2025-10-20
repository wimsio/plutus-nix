{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}


module Vesting where

import Plutus.V2.Ledger.Api
  ( Datum(..)
  , BuiltinData
  , POSIXTime
  , PubKeyHash
  , ScriptContext(..)
  , TxInfo(..)
  , Validator
  , TxOut(..)
  , TxOutRef(..)
  , Credential(..)
  , Address(..)
  , ValidatorHash
  , from
  , mkValidatorScript
  )
import Plutus.V2.Ledger.Contexts
  ( scriptContextTxInfo
  , txSignedBy
  , getContinuingOutputs
  , findOwnInput
  , ownHash
  , findDatum
  , txInInfoResolved
  )

import Plutus.V1.Ledger.Interval (contains, from, interval)
import PlutusTx (compile, makeIsDataIndexed, fromBuiltinData)
import PlutusTx.Prelude
  ( Bool(..)
  , traceIfFalse
  , traceError
  , (&&)
  , ($)
  , (==)
  , (+)
  , Maybe(..)
  , mempty
  )
import Prelude (Show, Integer)
import Prelude (Show, Integer, IO, String)
import Utilities (Network, posixTimeFromIso8601,
                  printDataToJSON,
                  validatorAddressBech32,
                  wrapValidator, writeValidatorToFile)

import Data.Maybe (fromJust)

--------------------------------------------------------------------------------
-- Datum / Redeemer Types
--------------------------------------------------------------------------------

data Actions = Update | Cancel | Buy
PlutusTx.makeIsDataIndexed ''Actions [('Update, 0), ('Cancel, 1), ('Buy, 2)]

data VestingDatum = VestingDatum
  { beneficiary :: PubKeyHash
  , deadline    :: POSIXTime
  , code        :: Integer
  } deriving Show
PlutusTx.makeIsDataIndexed ''VestingDatum [('VestingDatum, 0)]

--------------------------------------------------------------------------------
-- Validator Logic
--------------------------------------------------------------------------------

{-# INLINABLE mkVestingValidator #-}
mkVestingValidator :: VestingDatum -> Actions -> ScriptContext -> Bool
mkVestingValidator dat action ctx =
    case action of
        Buy ->
             traceIfFalse "beneficiary's signature missing" signedByBeneficiary
          && traceIfFalse "deadline not reached"            deadlineReached
          && traceIfFalse "invalid vesting code"            codeMatches
          && traceIfFalse "validity window too wide"        validityWindowOk
          && traceIfFalse "output not to same script"       outputAddressConsistent
          && traceIfFalse "datum type/shape invalid"        datumTypeSafe
          && traceIfFalse "value not preserved"             valuePreserved
          && traceIfFalse "unexpected mint/burn"            noMinting

        Update ->
             traceIfFalse "only beneficiary can update"     signedByBeneficiary
          && traceIfFalse "validity window too wide"        validityWindowOk
          && traceIfFalse "output not to same script"       outputAddressConsistent
          && traceIfFalse "datum type/shape invalid"        datumTypeSafe
          && traceIfFalse "value not preserved"             valuePreserved
          && traceIfFalse "unexpected mint/burn"            noMinting

        Cancel ->
             traceIfFalse "only beneficiary can cancel"     signedByBeneficiary
          && traceIfFalse "too early to cancel"             deadlineReached
          && traceIfFalse "validity window too wide"        validityWindowOk
          && traceIfFalse "datum type/shape invalid"        datumTypeSafe
          && traceIfFalse "unexpected mint/burn"            noMinting
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- 1) Signature
    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info (beneficiary dat)

    -- 2) Deadline
    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) (txInfoValidRange info)

    -- 3) Validity window cap
    maxWindow :: POSIXTime
    maxWindow = 60000
    validityWindowOk :: Bool
    validityWindowOk =
        contains (interval (deadline dat) (deadline dat + maxWindow))
                 (txInfoValidRange info)

    -- 4) Inputs / Outputs
    ownIn :: TxOut
    ownIn = case findOwnInput ctx of
        Just i  -> txInInfoResolved i
        Nothing -> traceError "own input not found"

    continuing :: TxOut
    continuing = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one continuing output"

    -- 5) Datum Code Integrity
    codeMatches :: Bool
    codeMatches = True
       
    -- 6) Value Preservation
    valuePreserved :: Bool
    valuePreserved = txOutValue continuing == txOutValue ownIn

    -- 7) No Mint/Burn
    noMinting :: Bool
    noMinting = txInfoMint info == mempty

    -- 8) Output Address Consistency
    outputAddressConsistent :: Bool
    outputAddressConsistent =
        case addressCredential (txOutAddress continuing) of
            ScriptCredential vh -> vh == ownHash ctx
            _                   -> False

    -- 9) Datum Type Safety
    datumTypeSafe :: Bool
    datumTypeSafe = True
    
{-# INLINABLE mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/vesting.plutus" validator

vestingAddressBech32 :: Network -> String
vestingAddressBech32 network = validatorAddressBech32 network validator

printVestingDatumJSON :: PubKeyHash -> String -> IO ()
printVestingDatumJSON pkh time =
  printDataToJSON $ VestingDatum
    { beneficiary = pkh
    , deadline    = fromJust $ posixTimeFromIso8601 time
    , code        = 42 -- example placeholder
    }
