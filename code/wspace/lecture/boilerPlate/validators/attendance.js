export const attendanceFile = `{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Attendance where

import PlutusTx
import PlutusTx.Prelude
import Prelude (IO)

import Plutus.V2.Ledger.Api
  ( BuiltinData, ScriptContext(..), TxInfo(..), TxOut(..), Address(..)
  , Credential(..), Validator, Datum(..), OutputDatum(..)
  , mkValidatorScript, ValidatorHash, Value
  )
import Plutus.V2.Ledger.Contexts
  ( txSignedBy, findOwnInput, getContinuingOutputs, scriptContextTxInfo, txOutAddress, txOutValue, txInfoSignatories )
import Plutus.V1.Ledger.Value
  ( AssetClass(..), assetClassValue )
import GHC.Generics (Generic)

------------------------------------------------------------
-- PARAMETERS (chosen off-chain and lifted on-chain)
------------------------------------------------------------

data AttendanceParams = AttendanceParams
  { apOracleAdmin     :: PubKeyHash    -- who may mark attendance
  , apTrancheSizeDays :: Integer       -- days per tranche (e.g., 5)
  , apRewardAsset     :: AssetClass    -- (CS, TN) of reward asset
  , apRewardPerTranche:: Integer       -- units of reward per tranche
  }
PlutusTx.unstableMakeIsData ''AttendanceParams
PlutusTx.makeLift               ''AttendanceParams

------------------------------------------------------------
-- DATUM: on-chain state per attendance UTxO
------------------------------------------------------------

data AttendanceDatum = AttendanceDatum
  { adUserAddress  :: PubKeyHash  -- user that accumulates days & may claim
  , adTotalDays    :: Integer     -- total days recorded so far
  , adTrancheLevel :: Integer     -- number of tranches already claimed
  }
  deriving stock (Generic)
PlutusTx.unstableMakeIsData ''AttendanceDatum

------------------------------------------------------------
-- REDEEMER: actions
------------------------------------------------------------

data AttendanceAction
  = MarkAttendance              -- admin-only: increments total days by 1
  | ClaimTranche                -- user-only: claims reward for newly-eligible tranches
  deriving stock (Generic)
PlutusTx.unstableMakeIsData ''AttendanceAction

------------------------------------------------------------
-- HELPERS
------------------------------------------------------------

{-# INLINABLE ownInputOut #-}
ownInputOut :: ScriptContext -> TxOut
ownInputOut ctx =
  case findOwnInput ctx of
    Just i  -> txInInfoResolved i
    Nothing -> traceError "own input not found"

{-# INLINABLE oneContinuingOut #-}
oneContinuingOut :: ScriptContext -> TxOut
oneContinuingOut ctx =
  case getContinuingOutputs ctx of
    [o] -> o
    _   -> traceError "expect exactly one continuing output"

{-# INLINABLE readInlineDatum #-}
readInlineDatum :: TxOut -> AttendanceDatum
readInlineDatum o =
  case txOutDatum o of
    OutputDatum (Datum d) -> unsafeFromBuiltinData d
    _                     -> traceError "expected inline datum"

{-# INLINABLE sameScript #-}
sameScript :: TxOut -> TxOut -> Bool
sameScript a b = txOutAddress a == txOutAddress b

{-# INLINABLE eligibleTranches #-}
eligibleTranches :: Integer -> Integer -> Integer
eligibleTranches totalDays trancheSize =
  if trancheSize <= 0 then traceError "bad tranche size"
  else totalDays \`divide\` trancheSize

{-# INLINABLE payoutValue #-}
payoutValue :: AssetClass -> Integer -> Value
payoutValue ac n = assetClassValue ac n

------------------------------------------------------------
-- VALIDATOR
------------------------------------------------------------

{-# INLINABLE mkAttendanceValidator #-}
mkAttendanceValidator
  :: AttendanceParams
  -> AttendanceDatum
  -> AttendanceAction
  -> ScriptContext
  -> Bool
mkAttendanceValidator params datum action ctx =
  case action of
    --------------------------------------------------------
    -- 1) Admin marks attendance: increment totalDays by 1
    --------------------------------------------------------
    MarkAttendance ->
         traceIfFalse "admin signature missing" (txSignedBy info (apOracleAdmin params))
      && traceIfFalse "must keep same script"   (sameScript inRef outRef)
      && traceIfFalse "value must be same"      (txOutValue inRef == txOutValue outRef)
      && traceIfFalse "bad updated datum"       checkUpdated

      where
        outRef = oneContinuingOut ctx
        newD   = readInlineDatum outRef

        checkUpdated =
             adUserAddress newD  == adUserAddress datum
          && adTrancheLevel newD == adTrancheLevel datum
          && adTotalDays newD    == (adTotalDays datum + 1)

    --------------------------------------------------------
    -- 2) User claims all newly eligible tranches at once
    --------------------------------------------------------
    ClaimTranche ->
         traceIfFalse "user signature missing" (txSignedBy info (adUserAddress datum))
      && traceIfFalse "must keep same script"  (sameScript inRef outRef)
      && traceIfFalse "no new tranche"         (newClaimed > 0)
      && traceIfFalse "bad updated datum"      checkUpdated
      && traceIfFalse "wrong value delta"      valueOK

      where
        outRef = oneContinuingOut ctx
        newD   = readInlineDatum outRef

        trancheSize = apTrancheSizeDays params
        already     = adTrancheLevel datum
        eligAll     = eligibleTranches (adTotalDays datum) trancheSize
        newLevel    = eligAll                         -- we force claiming up to max eligible
        newClaimed  = newLevel - already              -- how many tranches newly claimed

        expectedPayoutUnits :: Integer
        expectedPayoutUnits = newClaimed * apRewardPerTranche params

        -- value check: out value = in value - payout
        valueOK =
          let expectedOut = txOutValue inRef <> negate (payoutValue (apRewardAsset params) expectedPayoutUnits)
          in  txOutValue outRef == expectedOut

        checkUpdated =
             adUserAddress newD  == adUserAddress datum
          && adTotalDays newD    == adTotalDays datum               -- days unchanged when claiming
          && adTrancheLevel newD == newLevel

  where
    info  = scriptContextTxInfo ctx
    inRef = ownInputOut ctx

------------------------------------------------------------
-- UNTYPED WRAPPER & SPECIALIZATION
------------------------------------------------------------

{-# INLINABLE mkWrapped #-}
mkWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrapped p d r c =
  let params = unsafeFromBuiltinData p
      datum  = unsafeFromBuiltinData d
      act    = unsafeFromBuiltinData r
      ctx    = unsafeFromBuiltinData c
  in  check (mkAttendanceValidator params datum act ctx)
  where
    check True  = ()
    check False = traceError "attendance validation failed"

validator :: AttendanceParams -> Validator
validator ps =
  mkValidatorScript $
    $$(PlutusTx.compile [|| \ps' -> mkWrapped ps' ||])
      \`PlutusTx.applyCode\` PlutusTx.liftCode ps

validatorHash' :: AttendanceParams -> ValidatorHash
validatorHash' = validatorHash . validator

validatorAddress :: AttendanceParams -> Address
validatorAddress ps = Address (ScriptCredential (validatorHash' ps)) Nothing
------------------------------------------------------------
-- END OF FILE
------------------------------------------------------------
`