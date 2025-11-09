export const flashloansFile = `{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module FlashLoans  where

import           Prelude                    (IO, putStrLn, print, String, (<>))
import           PlutusTx                   (compile, liftCode)
import qualified PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           PlutusLedgerApi.V2         (BuiltinData, ScriptContext(..), Validator)
import qualified PlutusLedgerApi.V2         as V2
import qualified PlutusLedgerApi.V1.Value   as Value
import qualified PlutusLedgerApi.V2.Contexts as Contexts
import qualified PlutusLedgerApi.Common     as Common
import qualified PlutusLedgerApi.V2.Scripts as Scripts
import qualified Plutus.Script.Utils.V2.Scripts as Utils
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Typed
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Short      as SBS
import           Codec.Serialise            (serialise)
import qualified Cardano.Api                as Api
import qualified Cardano.Api.Shelley        as Shelley

--------------------------------------------------------------------------------
-- 1️⃣  Flash Loan Types (Params, Datum, Redeemer)
--------------------------------------------------------------------------------

data FlashParams = FlashParams
  { owner :: V2.PubKeyHash  -- Owner/admin of the validator
  } deriving Prelude.Show

PlutusTx.makeLift ''FlashParams

data FlashDatum = FlashDatum
  { borrower :: V2.PubKeyHash
  , amount   :: Integer
  } deriving Prelude.Show

PlutusTx.unstableMakeIsData ''FlashDatum

data FlashRedeemer = Borrow | Repay
  deriving Prelude.Show

PlutusTx.unstableMakeIsData ''FlashRedeemer

--------------------------------------------------------------------------------
-- 2️⃣  Validator Logic
--------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: FlashParams -> FlashDatum -> FlashRedeemer -> V2.ScriptContext -> Bool
mkValidator p d r ctx =
    case r of
      Borrow -> traceIfFalse "Borrower not signed" borrowerSigned
      Repay  -> traceIfFalse "Repayment not verified" repaymentOk
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    borrowerSigned :: Bool
    borrowerSigned = V2.txSignedBy info (borrower d)

    -- Simplified repayment check: ensures same borrower returns at least the same amount
    repaymentOk :: Bool
    repaymentOk =
        let inputsValue  = Contexts.valueSpent info
            outputsValue = Contexts.valuePaidTo info (owner p)
        in Value.geq outputsValue (Value.scale (amount d) (Value.lovelaceValueOf 1))

--------------------------------------------------------------------------------
-- 3️⃣  Validator Wrapper for BuiltinData
--------------------------------------------------------------------------------

{-# INLINABLE validatorWrapper #-}
validatorWrapper :: FlashParams -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
validatorWrapper p d r ctx _ =
    let datum      = PlutusTx.unsafeFromBuiltinData @FlashDatum d
        redeemer   = PlutusTx.unsafeFromBuiltinData @FlashRedeemer r
        context    = PlutusTx.unsafeFromBuiltinData @V2.ScriptContext ctx
    in if mkValidator p datum redeemer context
          then ()
          else traceError "FlashLoanValidator: validation failed"

--------------------------------------------------------------------------------
-- 4️⃣  Compile the Validator
--------------------------------------------------------------------------------

validator :: FlashParams -> Validator
validator p =
  PlutusTx.validatorToScript $
    $$(PlutusTx.compile [|| p' -> validatorWrapper p' ||])
    PlutusTx.applyCode PlutusTx.liftCode p

--------------------------------------------------------------------------------
-- 5️⃣  Write validator.plutus and print hash + address
--------------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator fp v = do
    let scriptSBS = SBS.toShort . LBS.toStrict $ serialise $ V2.unValidatorScript v
    SBS.writeFile fp scriptSBS
    putStrLn $ "✅ Validator written to: " <> fp

main :: IO ()
main = do
    let exampleOwner = "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
        params = FlashParams { owner = exampleOwner }
        val    = validator params

    writeValidator "validator.plutus" val

    let scriptHash = Utils.validatorHash val
    putStrLn $ "🔑 Validator Hash: " <> show scriptHash

    let addr = Utils.validatorAddress scriptHash Nothing
    putStrLn $ "🏦 Validator Address: " <> show addr

    putStrLn "✅ Done! Flash Loan validator compiled successfully."

    `