{-# LANGUAGE OverloadedStrings #-}

module CGPlutusUtilsv1
  ( bech32ToPubKeyHash
  , pkhToAddrB32
  , pkhToAddrB32Opt
  , pkhToAddrB32Testnet
  , pkhToAddrB32Mainnet
  ) where

import Codec.Binary.Bech32
  ( encodeLenient
  , dataPartFromBytes
  , dataPartToBytes
  , decodeLenient
  , humanReadablePartFromText
  )
import qualified Data.Text                  as T
import           Data.Bits                  ( (.|.), (.&.), shiftL, shiftR )
import           Data.Word                  ( Word8 )
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Base16     as B16
import qualified Plutus.V1.Ledger.Crypto    as Crypto
import qualified PlutusTx.Builtins.Class    as Builtins

-- | Decode a Shelley‑style Bech32 address to its payment PubKeyHash.
--   Only supports key‑based addresses (types 0,1,6).
bech32ToPubKeyHash :: String -> Either String Crypto.PubKeyHash
bech32ToPubKeyHash addrStr = do
  (_, dp) <- case decodeLenient (T.pack addrStr) of
    Left err -> Left $ "Bech32 decode error: " ++ show err
    Right d  -> Right d

  raw <- case dataPartToBytes dp of
    Nothing -> Left "Invalid Bech32 data part"
    Just b  -> Right b

  case BS.uncons raw of
    Nothing             -> Left "Empty address payload"
    Just (hdr, payload) ->
      let addrType = hdr `shiftR` 4 in
      if addrType `elem` [0,1,6]
        then if BS.length payload >= 28
          then let pkhBytes = BS.take 28 payload
               in Right $ Crypto.PubKeyHash $ Builtins.toBuiltin pkhBytes
          else Left "Payload too short for a 28‑byte PubKeyHash"
        else Left $ "Unsupported address type (header nibble = " ++ show addrType ++ ")"

-- | Convert a hex‑encoded 28‑byte PubKeyHash into a Shelley Bech32 enterprise address.
pkhToAddrB32
  :: String   -- ^ HRP (e.g. "addr", "addr_test")
  -> Word8    -- ^ network nibble (0=test, 1=main)
  -> String   -- ^ hex‑encoded PubKeyHash (56 hex chars)
  -> Either String String
pkhToAddrB32 hrpStr netId hexStr =
  case B16.decode (C.pack hexStr) of
    Left err -> Left $ "Hex decode error: " ++ err
    Right rawHash ->
      if BS.length rawHash /= 28
        then Left $ "Decoded length incorrect: " ++ show (BS.length rawHash) ++ " bytes"
        else case humanReadablePartFromText (T.pack hrpStr) of
          Left err -> Left $ "Invalid HRP: " ++ show err
          Right hrp ->
            let header  = (6 `shiftL` 4) .|. (netId .&. 0x0F)
                payload = BS.cons header rawHash
                dp      = dataPartFromBytes payload
                encoded = encodeLenient hrp dp
            in Right (T.unpack encoded)

-- | Optional HRP/network version. Defaults to ("addr_test",0).
pkhToAddrB32Opt
  :: Maybe String  -- ^ optional HRP
  -> Maybe Word8   -- ^ optional network nibble
  -> String        -- ^ PubKeyHash hex
  -> Either String String
pkhToAddrB32Opt mHrp mNetId hexStr =
  let hrp   = maybe "addr_test" id mHrp
      netId = maybe 0 id mNetId
  in pkhToAddrB32 hrp netId hexStr

-- | Shortcut for testnet: HRP="addr_test", netId=0.
pkhToAddrB32Testnet :: String -> Either String String
pkhToAddrB32Testnet = pkhToAddrB32 "addr_test" 0

-- | Shortcut for mainnet: HRP="addr", netId=1.
pkhToAddrB32Mainnet :: String -> Either String String
pkhToAddrB32Mainnet = pkhToAddrB32 "addr" 1