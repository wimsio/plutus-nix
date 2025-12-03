export const escrowFile = `{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module Escrow where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

-- Plutus core
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

-- Cardano API (for Bech32 address)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------

data EscrowDatum = EscrowDatum
    { edBuyer    :: PubKeyHash
    , edSeller   :: PubKeyHash
    , edAmount   :: Integer
    , edDeadline :: POSIXTime
    , edCurrency :: CurrencySymbol
    , edToken    :: TokenName
    }
PlutusTx.unstableMakeIsData ''EscrowDatum

data EscrowAction = PaySeller | RefundSeller
PlutusTx.unstableMakeIsData ''EscrowAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE scriptInputContainsNFT #-}
scriptInputContainsNFT :: ScriptContext -> CurrencySymbol -> TokenName -> Bool
scriptInputContainsNFT ctx cs tn =
    case findOwnInput ctx of
        Nothing -> traceError "no input from script found"
        Just i  ->
            let v = txOutValue $ txInInfoResolved i
            in valueOf v cs tn >= 1

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: EscrowDatum -> EscrowAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
      PaySeller ->
           traceIfFalse "script input missing NFT" (scriptInputContainsNFT ctx (edCurrency dat) (edToken dat)) &&
           traceIfFalse "buyer signature missing"   (txSignedBy info (edBuyer dat)) &&
           traceIfFalse "seller not paid"           sellerPaid &&
           traceIfFalse "buyer not receive NFT"     buyerGetsNFT
      RefundSeller ->
           traceIfFalse "script input missing NFT" (scriptInputContainsNFT ctx (edCurrency dat) (edToken dat)) &&
           traceIfFalse "seller signature missing" (txSignedBy info (edSeller dat)) &&
           traceIfFalse "too early for refund"     afterDeadline &&
           traceIfFalse "seller did not receive NFT" sellerGetsNFT
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txRange :: POSIXTimeRange
    txRange = txInfoValidRange info

    afterDeadline :: Bool
    afterDeadline = Interval.contains (Interval.from (edDeadline dat + 1)) txRange

    sellerPaid :: Bool
    sellerPaid =
      let v = valuePaidTo info (edSeller dat)
      in valueOf v adaSymbol adaToken >= edAmount dat

    buyerGetsNFT :: Bool
    buyerGetsNFT =
      let v = valuePaidTo info (edBuyer dat)
      in valueOf v (edCurrency dat) (edToken dat) >= 1

    sellerGetsNFT :: Bool
    sellerGetsNFT =
      let v = valuePaidTo info (edSeller dat)
      in valueOf v (edCurrency dat) (edToken dat) >= 1

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @EscrowDatum d
        red = unsafeFromBuiltinData @EscrowAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Validator Hash + Addresses
------------------------------------------------------------------------

-- On-chain (Plutus) hash and address
-- Compute validator hash using only plutus-ledger-api + plutus-tx
plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash validator =
    let bytes    = Serialise.serialise validator
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short              -- convert ShortByteString → ByteString
        builtin  = Builtins.toBuiltin strictBS      -- convert ByteString → BuiltinByteString
    in PlutusV2.ValidatorHash builtin

-- Derive the Plutus script address from the hash
plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing



-- Off-chain (Cardano API) Bech32 address for CLI use
-- Off-chain (Cardano API) Bech32 address for CLI use
toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised

        scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)

        -- The type annotation declares it's a Babbage-era address
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)


------------------------------------------------------------------------
-- File writing
------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "validator.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator


    putStrLn "
--- Escrow NFT Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "Escrow NFT validator generated successfully."
`