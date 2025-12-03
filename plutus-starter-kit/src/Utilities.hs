{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module Utilities
       (toBech32ScriptAddress
       , writeValidator
       , mkValidatorUntyped
       , validator
       , plutusValidatorHash
       , plutusScriptAddress
       , network
       )   where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Data.Text as T
import Plutus.V2.Ledger.Api
-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
-- Cardano API (for Bech32 address)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx as PlutusTx
import Plutus.V2.Ledger.Api
    ( Validator
    , Address(..)
    , Credential(..)
    , ScriptContext
    , BuiltinData
    , mkValidatorScript
    )
import qualified Plutus.V2.Ledger.Api as PlutusV2
import qualified Codec.Serialise      as Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified PlutusTx.Builtins    as Builtins
import ValidatorLogic
import qualified Cardano.Api as C
network :: C.NetworkId
network = C.Testnet (C.NetworkMagic 1) 

------------------------------------------------------------------------
-- Untyped wrapper
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = PlutusTx.unsafeFromBuiltinData @CoxyDatum   d
        red = PlutusTx.unsafeFromBuiltinData @CoxyRedeemer  r
        ctx = PlutusTx.unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx
       then ()
       else error ()

------------------------------------------------------------------------
-- Compiled validator
------------------------------------------------------------------------

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Hash + script address
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash v =
    let bytes    = Serialise.serialise v
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress =
   Address (ScriptCredential (plutusValidatorHash validator)) Nothing

------------------------------------------------------------------------
-- Validator Hash + Addresses
------------------------------------------------------------------------

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
    

  
