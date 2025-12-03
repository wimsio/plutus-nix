{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module MoranNFT where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

-- Plutus
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..))
import qualified PlutusTx.Builtins as Builtins
import qualified PlutusTx.AssocMap as Map

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString as BS

-- Cardano API
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS


-------------------------------------------------------------------------
-- Shared flattenValue from your working contract
-------------------------------------------------------------------------
{-# INLINABLE flattenValue #-}
flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]
flattenValue (Value m) =
    concatMap (\(cs, mp) -> [(cs, tn, amt) | (tn, amt) <- Map.toList mp])
              (Map.toList m)


-------------------------------------------------------------------------
-- NFT Minting Policy (one-time mint)
-------------------------------------------------------------------------
{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkNFTPolicy oref tn () ctx =
    traceIfFalse "UTxO not consumed"   hasUTxO           &&
    traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) (txInfoInputs info)

    checkMintedAmount :: Bool
    checkMintedAmount =
        case flattenValue (txInfoMint info) of
            [(cs', tn', amt)] -> tn' == tn && amt == 1
            _                 -> False


-------------------------------------------------------------------------
-- Wrapper
-------------------------------------------------------------------------
{-# INLINABLE mkWrappedNFTPolicy #-}
mkWrappedNFTPolicy
    :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTPolicy tid ix tn' _redeemer ctx =
    let oref = TxOutRef
                    (TxId (unsafeFromBuiltinData tid))
                    (unsafeFromBuiltinData ix)
        tn   = unsafeFromBuiltinData tn'
    in if mkNFTPolicy oref tn () (unsafeFromBuiltinData ctx)
       then ()
       else error ()


nftCode :: CompiledCode
    (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
nftCode = $$(PlutusTx.compile [|| mkWrappedNFTPolicy ||])


nftPolicy :: TxOutRef -> TokenName -> MintingPolicy
nftPolicy oref tn =
    mkMintingPolicyScript $
        nftCode
            `applyCode` liftCode (toBuiltinData (getTxId $ txOutRefId oref))
            `applyCode` liftCode (toBuiltinData (txOutRefIdx oref))
            `applyCode` liftCode (toBuiltinData tn)


-------------------------------------------------------------------------
-- Currency Symbol (same style as your rebase code)
-------------------------------------------------------------------------
{-# INLINABLE currencySymbol #-}
currencySymbol :: MintingPolicy -> CurrencySymbol
currencySymbol pol =
    let bytes   = LBS.toStrict (Serialise.serialise pol)
        builtin = Builtins.toBuiltin bytes
        hash    = sha2_256 builtin
    in CurrencySymbol hash


-------------------------------------------------------------------------
-- Convert minting policy → Bech32 address (same as your working script)
-------------------------------------------------------------------------
toBech32PolicyAddress :: C.NetworkId -> MintingPolicy -> String
toBech32PolicyAddress network pol =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise pol
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash   = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)


-------------------------------------------------------------------------
-- File Saving
-------------------------------------------------------------------------
writePolicy :: FilePath -> MintingPolicy -> IO ()
writePolicy fp pol = do
    LBS.writeFile fp (Serialise.serialise pol)
    putStrLn $ "NFT policy written to: " <> fp


-------------------------------------------------------------------------
-- MAIN (Generates the MORAN NFT)
-------------------------------------------------------------------------
main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    ---------------------------------------------------------------------
    -- CHANGE THIS TO YOUR MINTING UTXO REF
    ---------------------------------------------------------------------
    let oref = TxOutRef
                (TxId "4a74a76648a02a30f7338647f102995ffd0620d497677d836d871b082bb5c9b1")
                2

    let tn = TokenName "Moran"

    let pol = nftPolicy oref tn

    writePolicy "moran-nft.plutus" pol

    let cs   = currencySymbol pol
    let addr = toBech32PolicyAddress network pol

    putStrLn "\n--- MORAN NFT POLICY ---"
    putStrLn $ "Token Name: Moran"
    putStrLn $ "Currency Symbol: " <> P.show cs
    putStrLn $ "Script Address: " <> addr
    putStrLn "---------------------------------"
    putStrLn "Moran NFT policy generated."
