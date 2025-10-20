{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Vesting where

import           Data.Maybe                (fromJust)
import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api      (BuiltinData, POSIXTime, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo (txInfoValidRange),
                                            Validator, from, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx                  (compile, makeIsDataIndexed, unstableMakeIsData)
import           PlutusTx.Prelude          (Bool, traceIfFalse, ($), (&&))
import           Prelude                   (IO, String, Integer, Show)
import           Utilities                 (Network, posixTimeFromIso8601,
                                            printDataToJSON,
                                            validatorAddressBech32,
                                            wrapValidator, writeValidatorToFile)
                                            
---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    , code        :: Integer
    } deriving (Show)

unstableMakeIsData ''VestingDatum

data Actions = Update | Cancel | Buy deriving(Show)
PlutusTx.makeIsDataIndexed ''Actions [('Update, 0), ('Cancel, 1), ('Buy, 2)]

{-# INLINABLE mkVestingValidator #-}
mkVestingValidator :: VestingDatum -> Actions -> ScriptContext -> Bool
mkVestingValidator dat action ctx =
  case action of
    Buy ->
         traceIfFalse "beneficiary's signature missing"        signedByBeneficiary
      && traceIfFalse "deadline not reached"                   deadlineReached
      && traceIfFalse "invalid vesting code (double spend?)"   codeMatches
      && traceIfFalse "validity window too wide"               validityWindowOk
      && traceIfFalse "output not to same script"              outputAddressConsistent
      && traceIfFalse "datum type/shape invalid"               datumTypeSafe
      && traceIfFalse "value not preserved"                    valuePreserved
      && traceIfFalse "unexpected mint/burn"                   noMinting

    Update ->
         traceIfFalse "only beneficiary can update"            signedByBeneficiary
      && traceIfFalse "validity window too wide"               validityWindowOk
      && traceIfFalse "output not to same script"              outputAddressConsistent
      && traceIfFalse "datum type/shape invalid"               datumTypeSafe
      && traceIfFalse "value not preserved"                    valuePreserved
      && traceIfFalse "unexpected mint/burn"                   noMinting

    Cancel ->
         traceIfFalse "only beneficiary can cancel"            signedByBeneficiary
      && traceIfFalse "too early to cancel"                    deadlineReached
      && traceIfFalse "validity window too wide"               validityWindowOk
      && traceIfFalse "datum type/shape invalid"               datumTypeSafe
      && traceIfFalse "unexpected mint/burn"                   noMinting
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- 1) Signature
    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info (beneficiary dat)

    -- 2) Deadline (temporal safety)
    deadlineReached :: Bool
    deadlineReached =
      contains (from $ deadline dat) (txInfoValidRange info)

    -- 3) Maximum validity window (cap range length by tying it to [deadline, deadline + maxWindow])
    --    This avoids having to deconstruct Interval bounds on-chain.
    maxWindow :: POSIXTime
    maxWindow = 60000 -- e.g. 60 seconds; tune to your protocol
    validityWindowOk :: Bool
    validityWindowOk =
      contains (interval (deadline dat) (deadline dat + maxWindow))
               (txInfoValidRange info)

    -- Helpers to get own input and the single continuing output we expect
    ownIn :: TxOut
    ownIn = case findOwnInput ctx of
      Just i  -> txInInfoResolved i
      Nothing -> traceError "own input not found"

    continuing :: TxOut
    continuing =
      case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one continuing output"

    -- 4) Datum Code Integrity (prevents replay / double-spend via mismatched state)
    codeMatches :: Bool
    codeMatches = case txOutDatum continuing of
      OutputDatum (Datum d) ->
        case PlutusTx.fromBuiltinData d of
          Just (VestingDatum _ _ cOut) -> cOut == code dat
          Nothing                      -> traceError "invalid output datum"
      _ -> traceError "expected inline datum"

    -- 5) Value Preservation (keep value at script unchanged across state updates)
    --    For flows that should *not* keep a continuing output (e.g., finalizing),
    --    gate this by action or adjust to your protocol economics.
    valuePreserved :: Bool
    valuePreserved = txOutValue continuing == txOutValue ownIn

    -- 6) Token Authenticity — default: no mint/burn allowed in these transitions
    noMinting :: Bool
    noMinting = txInfoMint info == mempty

    -- 7) Output Address Consistency — the continuing UTxO must remain at the same script
    outputAddressConsistent :: Bool
    outputAddressConsistent =
      case addressCredential (txOutAddress continuing) of
        ScriptCredential vh -> vh == ownHash ctx
        _                   -> False

    -- 8) Datum Type Safety — ensures the new continuing datum is the expected shape
    datumTypeSafe :: Bool
    datumTypeSafe = case txOutDatum continuing of
      OutputDatum (Datum d) ->
        case PlutusTx.fromBuiltinData d of
          Just (_ :: VestingDatum) -> True
          Nothing                  -> False
      _ -> False


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
