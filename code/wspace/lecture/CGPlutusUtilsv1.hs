{-# LANGUAGE OverloadedStrings #-}

module CGPlutusUtilsv1
  ( bech32ToPubKeyHash
  , pkhToAddrB32
  , pkhToAddrB32Opt
  , pkhToAddrB32Testnet
  , pkhToAddrB32Mainnet
  , decodeBech32Address
  , AddressInfo(..)
  , rebuildBaseAddress
  , pkhStrToPkh
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
import Plutus.V1.Ledger.Crypto (PubKeyHash(..))
import PlutusTx.Builtins (toBuiltin)

--------------------------------------------------------------------------------
-- Simple Bech32 Address to PubKeyHash Extractor (only payment key)
--------------------------------------------------------------------------------

-- | Decode Shelley-style Bech32 address to its payment PubKeyHash.
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
          else Left "Payload too short for a 28-byte PubKeyHash"
        else Left $ "Unsupported address type (header nibble = " ++ show addrType ++ ")"

--------------------------------------------------------------------------------
-- Round-trip decoder for payment + stake credentials
--------------------------------------------------------------------------------

-- | Extended address decoder that handles enterprise and base addresses.
data AddressInfo
  = EnterpriseAddr Crypto.PubKeyHash
  | BaseAddr Crypto.PubKeyHash BS.ByteString  -- ^ stake credential (28 bytes)
  deriving Show

decodeBech32Address :: String -> Either String AddressInfo
decodeBech32Address addrStr = do
  (_, dp) <- case decodeLenient (T.pack addrStr) of
    Left err -> Left $ "Bech32 decode error: " ++ show err
    Right d  -> Right d

  raw <- maybe (Left "Invalid Bech32 data part") Right (dataPartToBytes dp)
  case BS.uncons raw of
    Nothing -> Left "Empty address payload"
    Just (hdr, rest) ->
      let addrType = hdr `shiftR` 4 in
      case addrType of
        0 -> parseBase rest
        1 -> parseBase rest
        6 -> parseEnterprise rest
        _ -> Left $ "Unsupported address type (header nibble = " ++ show addrType ++ ")"
  where
    parseEnterprise bs =
      if BS.length bs >= 28
        then Right $ EnterpriseAddr (Crypto.PubKeyHash $ Builtins.toBuiltin (BS.take 28 bs))
        else Left "Payload too short for enterprise address"
    parseBase bs =
      if BS.length bs >= 56
        then let (pay, stake) = BS.splitAt 28 bs
              in Right $ BaseAddr (Crypto.PubKeyHash $ Builtins.toBuiltin pay) stake
        else Left "Payload too short for base address"

--------------------------------------------------------------------------------
-- pkhStrToPkh :: String -> Either String PubKeyHash
--------------------------------------------------------------------------------
-- | Convert a hex-encoded string (e.g. "096f22...") into a Plutus PubKeyHash
pkhStrToPkh :: String -> PubKeyHash
pkhStrToPkh hexStr =
    case  B16.decode (C.pack hexStr) of
      Right decoded -> PubKeyHash (toBuiltin decoded)
      Left err      -> error $ "Invalid hex input for PubKeyHash: " ++ err

--------------------------------------------------------------------------------
-- Bech32 address construction
--------------------------------------------------------------------------------

-- | Construct enterprise address from PubKeyHash.
pkhToAddrB32
  :: String   -- ^ HRP (e.g. "addr", "addr_test")
  -> Word8    -- ^ network nibble (0=test, 1=main)
  -> String   -- ^ hex-encoded PubKeyHash (56 hex chars)
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

-- | Reconstruct a base address from its components.
rebuildBaseAddress
  :: String      -- ^ HRP (e.g. "addr", "addr_test")
  -> Word8       -- ^ Address type: 0=key/key, 1=key/script
  -> Word8       -- ^ Network id (0 or 1)
  -> BS.ByteString  -- ^ 28-byte payment key hash
  -> BS.ByteString  -- ^ 28-byte stake credential
  -> Either String String
rebuildBaseAddress hrpStr addrType netId payKeyHash stakeCred = do
  hrp <- either (Left . show) Right $ humanReadablePartFromText (T.pack hrpStr)
  let hdr     = (addrType `shiftL` 4) .|. (netId .&. 0x0F)
      payload = BS.cons hdr (payKeyHash <> stakeCred)
      dp      = dataPartFromBytes payload
  return $ T.unpack $ encodeLenient hrp dp


