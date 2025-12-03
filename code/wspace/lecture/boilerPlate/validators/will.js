export const willFile = `{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Will where

import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Prelude (IO, print)
import GHC.Generics (Generic)

import Plutus.V2.Ledger.Api
  ( BuiltinData, ScriptContext(..), TxInfo(..), POSIXTime, PubKeyHash
  , Validator, mkValidatorScript, ValidatorHash, Address(..), Credential(..)
  , OutputDatum(..), Datum(..), TxOut(..)
  , CurrencySymbol, TokenName
  )
import Plutus.V2.Ledger.Contexts
  ( scriptContextTxInfo, txInfoValidRange, txInfoOutputs, txSignedBy
  , valuePaidTo, getContinuingOutputs
  )
import Plutus.V1.Ledger.Interval (from, to, contains)
import Plutus.V1.Ledger.Value (valueOf)

--------------------------------------------------------------------------------
-- ON-CHAIN TYPES
--------------------------------------------------------------------------------

-- | A single asset specification (policy, token, quantity)
data AssetSpec = AssetSpec
  { asCS  :: CurrencySymbol
  , asTN  :: TokenName
  , asQty :: Integer
  }
  deriving stock (Generic)
PlutusTx.unstableMakeIsData ''AssetSpec
PlutusTx.makeLift         ''AssetSpec

-- | Beneficiary with a bundle of assets
data Beneficiary = Beneficiary
  { bPkh    :: PubKeyHash
  , bAssets :: [AssetSpec]
  }
  deriving stock (Generic)
PlutusTx.unstableMakeIsData ''Beneficiary
PlutusTx.makeLift         ''Beneficiary

-- | Parameters set at instantiation time (this is your Will)
data WillParams = WillParams
  { wpTestator     :: PubKeyHash      -- who can revoke before unlock
  , wpExecutor     :: PubKeyHash      -- who executes the will on/after unlock
  , wpUnlockTime   :: POSIXTime       -- earliest time the will can be executed
  , wpPlan         :: [Beneficiary]   -- who gets what
  }
  deriving stock (Generic)
PlutusTx.unstableMakeIsData ''WillParams
PlutusTx.makeLift         ''WillParams

-- | Minimal datum/redeemer: we don’t need state, just actions.
data WillDatum = WillDatum
PlutusTx.unstableMakeIsData ''WillDatum

data WillAction = Execute | Revoke
PlutusTx.unstableMakeIsData ''WillAction

--------------------------------------------------------------------------------
-- VALIDATOR LOGIC
--------------------------------------------------------------------------------

{-# INLINABLE mkWillValidator #-}
mkWillValidator :: WillParams -> WillDatum -> WillAction -> ScriptContext -> Bool
mkWillValidator params _ action ctx =
  case action of
    -- Executor distributes to beneficiaries on/after unlock time.
    Execute ->
         traceIfFalse "executor signature missing"   (txSignedBy info (wpExecutor params))
      && traceIfFalse "too early to execute"         (isOnOrAfter (wpUnlockTime params) info)
      && traceIfFalse "plan not satisfied"           (planSatisfied info (wpPlan params))
      && traceIfFalse "must close script (no cont)"  (null (getContinuingOutputs ctx))

    -- Testator can revoke (spend) before unlock time for any reason.
    Revoke  ->
         traceIfFalse "not testator"                 (txSignedBy info (wpTestator params))
      && traceIfFalse "too late to revoke"           (isStrictlyBefore (wpUnlockTime params) info)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

-- | Check the lower-bound time: tx valid range ⊆ [unlock, +∞)
{-# INLINABLE isOnOrAfter #-}
isOnOrAfter :: POSIXTime -> TxInfo -> Bool
isOnOrAfter t info = contains (from t) (txInfoValidRange info)

-- | Check the tx occurs strictly before unlock: tx valid range ⊆ (-∞, unlock)
-- Note: 'to t' is an interval up-to (exclusive) t.
{-# INLINABLE isStrictlyBefore #-}
isStrictlyBefore :: POSIXTime -> TxInfo -> Bool
isStrictlyBefore t info = contains (to t) (txInfoValidRange info)

-- | Verify that each beneficiary has been paid at least the specified assets.
{-# INLINABLE planSatisfied #-}
planSatisfied :: TxInfo -> [Beneficiary] -> Bool
planSatisfied info bs = all (beneficiaryPaid info) bs

{-# INLINABLE beneficiaryPaid #-}
beneficiaryPaid :: TxInfo -> Beneficiary -> Bool
beneficiaryPaid info (Beneficiary pkh specs) =
  let received = valuePaidTo info pkh
  in all (\(AssetSpec cs tn q) -> valueOf received cs tn >= q) specs

--------------------------------------------------------------------------------
-- UNTYPED WRAPPER
--------------------------------------------------------------------------------

{-# INLINABLE mkWrapped #-}
mkWrapped :: WillParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrapped params d r c =
  let _datum  = unsafeFromBuiltinData d :: WillDatum
      action  = unsafeFromBuiltinData r :: WillAction
      ctx     = unsafeFromBuiltinData c :: ScriptContext
  in check (mkWillValidator params _datum action ctx)
  where
    check True  = ()
    check False = traceError "will validation failed"

--------------------------------------------------------------------------------
-- COMPILED PARAMETERISED VALIDATOR
--------------------------------------------------------------------------------

validator :: WillParams -> Validator
validator params =
  mkValidatorScript
    ( $$(PlutusTx.compile [|| \ps -> mkWrapped ps ||])
        \`PlutusTx.applyCode\` PlutusTx.liftCode params
    )

validatorHash' :: WillParams -> ValidatorHash
validatorHash' = validatorHash . validator

validatorAddress :: WillParams -> Address
validatorAddress ps = Address (ScriptCredential (validatorHash' ps)) Nothing

--------------------------------------------------------------------------------
-- MAIN (for a friendly compile message off-chain)
--------------------------------------------------------------------------------

main :: IO ()
main = print "✅ Will validator compiled successfully!"
`