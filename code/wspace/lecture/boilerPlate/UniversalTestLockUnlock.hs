{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Simple "time-locked to a beneficiary" validator built on top of the universal rules engine.
--   Rule: spend is signed by beneficiary AND after deadline.
module UniversalTestLockUnlock
  ( mkLockByDeadlineValidator
  , mkLockByDeadlineParams
  , serialiseToCBOR
  , writeValidatorCBOR
  , pkhFromHexUnsafe
  , plutus
  ) where

-- Plutus
import           Plutus.V2.Ledger.Api
  ( Validator, POSIXTime, PubKeyHash(..), unValidatorScript )
import           PlutusTx.Prelude ()  -- keep NoImplicitPrelude happy

-- Base / IO (explicit because NoImplicitPrelude)
import           Prelude (FilePath, IO, String, error, Either(..), (*), ($) )

-- Serialisation & bytes
import qualified Codec.Serialise          as Serialise
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.ByteString.Char8    as C     -- for hex decode input
import qualified Data.ByteString.Char8    as C8    -- for JSON building
import qualified Data.ByteString.Base16   as B16

-- Builtins
import qualified PlutusTx.Builtins        as Builtins

-- Your universal engine
import           Universal
  ( Params(..)
  , SpendingSpec(..)
  , MintingSpec(..)
  , Rule(..)
  , mkSpendingValidator
  )

--------------------------------------------------------------------------------
-- Params for "lock by deadline, signed by beneficiary"
--------------------------------------------------------------------------------

mkLockByDeadlineParams :: PubKeyHash -> POSIXTime -> Params
mkLockByDeadlineParams beneficiaryPKH deadline =
  let spendingRules =
        [ RequireAnySigner [beneficiaryPKH]
        , ValidAfter deadline
        ]
  in Params
      { sSpec = SpendingSpec spendingRules
      , mSpec = MintingSpec []     -- no mint rules
      , extra = []                 -- spare param slots
      }

mkLockByDeadlineValidator :: PubKeyHash -> POSIXTime -> Validator
mkLockByDeadlineValidator pkh dl =
  mkSpendingValidator (mkLockByDeadlineParams pkh dl)

--------------------------------------------------------------------------------
-- Serialisation helpers (raw CBOR + text envelope)
--------------------------------------------------------------------------------

-- Lazy CBOR from a Validator
serialiseToCBOR :: Validator -> LBS.ByteString
serialiseToCBOR v =
  let scr = unValidatorScript v
  in Serialise.serialise scr

writeValidatorCBOR :: FilePath -> Validator -> IO ()
writeValidatorCBOR fp v = LBS.writeFile fp (serialiseToCBOR v)

-- Strict CBOR bytes from a Validator
serialiseToCBORBytes :: Validator -> BS.ByteString
serialiseToCBORBytes v = LBS.toStrict (serialiseToCBOR v)

-- Write cardano-cli compatible text envelope JSON:
-- {
--   "type": "PlutusScriptV2",
--   "description": "",
--   "cborHex": "<hex>"
-- }
writeValidatorEnvelope :: FilePath -> Validator -> IO ()
writeValidatorEnvelope fp v = do
  let cbor = serialiseToCBORBytes v                  -- BS.ByteString (strict)
      hex  = B16.encode cbor                         -- BS.ByteString (strict)
      json = LBS.fromStrict $
               C8.concat
                 [ C8.pack "{\n  \"type\": \"PlutusScriptV2\",\n  \"description\": \"\",\n  \"cborHex\": \""
                 , hex
                 , C8.pack "\"\n}\n"
                 ]
  LBS.writeFile fp json

--------------------------------------------------------------------------------
-- Hex → PubKeyHash (unsafe, for quick tests)
--------------------------------------------------------------------------------

pkhFromHexUnsafe :: String -> PubKeyHash
pkhFromHexUnsafe hex =
  case B16.decode (C.pack hex) of
    Right raw -> PubKeyHash (Builtins.toBuiltin raw)
    Left  _   -> error "pkhFromHexUnsafe: invalid hex"

-- Example inputs (replace as needed)
pkh :: PubKeyHash
pkh = pkhFromHexUnsafe "d57bb0cffa16332aa214bbbf5de72934c9a499fe1dc0d4ff223270a5"

pTime :: POSIXTime
pTime = 1762758828 * 1000    -- POSIXTime is milliseconds

plutus :: IO ()
plutus =
  writeValidatorEnvelope "./assets/lock-by-deadline.plutus"
    (mkLockByDeadlineValidator pkh pTime)

-- If this file is used as an executable entrypoint:
main :: IO ()
main = plutus
