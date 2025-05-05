{-# LANGUAGE OverloadedStrings #-}

module CGPlutusUtils
  ( -- * Convert Bech32 → PubKeyHash
    bech32ToPubKeyHash
  ) where

import qualified Codec.Binary.Bech32    as Bech32
import qualified Data.Text              as T
import           Data.Bits              (shiftR)
import qualified Data.ByteString        as BS
import qualified Plutus.V1.Ledger.Crypto as Crypto
import qualified PlutusTx.Builtins.Class as Builtins

-- | Decode a Shelley‑style Bech32 address and extract its payment PubKeyHash.
--   Only supports key‑based addresses (types 0,1,6). Fails on Byron or script‑only addrs.
bech32ToPubKeyHash :: String -> Either String Crypto.PubKeyHash
bech32ToPubKeyHash addrStr = do
    -- 1) Bech32 decode
    (_, dp) <- case Bech32.decodeLenient (T.pack addrStr) of
      Left err      -> Left $ "Bech32 decode error: " ++ show err
      Right decoded -> Right decoded

    -- 2) DataPart → raw payload bytes
    raw <- case Bech32.dataPartToBytes dp of
      Nothing -> Left "Invalid Bech32 data part"
      Just b  -> Right b

    -- 3) Split off header byte + payload
    case BS.uncons raw of
      Nothing              -> Left "Empty address payload"
      Just (hdr, payload)  ->
        let addrType = hdr `shiftR` 4
        in if addrType `elem` [0,1,6]
           then if BS.length payload >= 28
                then let pkhBytes = BS.take 28 payload
                     in Right $ Crypto.PubKeyHash $ Builtins.toBuiltin pkhBytes
                else Left "Payload too short to contain a 28‑byte PubKeyHash"
           else
             Left $ "Unsupported address type (header nibble = " ++ show addrType ++ ")"

