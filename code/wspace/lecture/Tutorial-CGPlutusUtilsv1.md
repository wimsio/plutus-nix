# Detailed Tutorial: Understanding and Using `CGPlutusUtilsv1.hs`

This tutorial provides a thorough overview of the `CGPlutusUtilsv1.hs` module, highlighting its imports, key functionalities, practical use cases, and recommended best practices. The module specializes in manipulating and interpreting Cardano Bech32 addresses.

## 1. Imports Overview

### Bech32 Encoding and Decoding:

* **Codec.Binary.Bech32:**

  * Functions for encoding, decoding, and manipulating Bech32 addresses.

### Data Manipulation and Conversion:

* **Data.Text:**

  * Essential for text processing.
* **Data.ByteString and Data.ByteString.Base16:**

  * Handle binary data manipulation and hexadecimal encoding/decoding.
* **Data.Bits and Data.Word:**

  * For bitwise operations required in parsing addresses.

### Crypto and Builtin Types:

* **Plutus.V1.Ledger.Crypto:**

  * Provides cryptographic types like `PubKeyHash`.
* **PlutusTx.Builtins.Class:**

  * Facilitates conversion to Plutus built-in types.

## 2. Core Functions and Types

### Address Extraction and Conversion

* **bech32ToPubKeyHash:**

  * Decodes a Shelley-style Bech32 address to its payment `PubKeyHash`.

* **decodeBech32Address:**

  * Decodes addresses, distinguishing between enterprise and base addresses.

### Address Construction

* **pkhToAddrB32:**

  * Constructs a Bech32 enterprise address from a `PubKeyHash`.

* **pkhToAddrB32Opt:**

  * Constructs a Bech32 address with optional parameters (HRP and network ID).

* **pkhToAddrB32Testnet/Mainnet:**

  * Convenience functions for testnet or mainnet address creation.

* **rebuildBaseAddress:**

  * Reconstructs a base address from payment and stake credentials.

## 3. Data Type

* **AddressInfo:**

  * Represents parsed address information, distinguishing between enterprise and base addresses with respective credentials.

## 4. Practical Usage Examples

```haskell
-- Decode Bech32 address to PubKeyHash
case bech32ToPubKeyHash "addr_test1..." of
  Left err -> putStrLn $ "Error: " ++ err
  Right pkh -> print pkh

-- Create testnet address from hex PubKeyHash
case pkhToAddrB32Testnet "659ad08ff1..." of
  Left err -> putStrLn $ "Error: " ++ err
  Right addr -> print addr
```

## 5. Testing Strategy

* Utilize rigorous unit testing for address decoding and construction scenarios.
* Validate round-trip conversions of addresses to ensure fidelity.
* Regularly verify handling of incorrect inputs to ensure error handling robustness.

## 6. Best Practices

* Always handle potential errors explicitly and provide meaningful messages.
* Document clearly any supported or unsupported address formats.
* Conduct comprehensive testing to prevent regressions and maintain robustness.

## 7. Summary

Here's a detailed and structured explanation of each component within the `CGPlutusUtilsv1.hs` module, enriched with clear source-code examples to illustrate usage.

---

# Detailed Explanation of `CGPlutusUtilsv1.hs`

This module provides utility functions to interact with Shelley-style Bech32 addresses and public key hashes (PKHs) within Cardano Plutus smart contracts.

---

## 1. Language Extensions

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

* **OverloadedStrings:** Allows strings literals (`"..."`) to be interpreted flexibly as different string-like types, such as `Text` or `ByteString`.

---

## 2. Imports and Their Usage

### Codec.Binary.Bech32

```haskell
import Codec.Binary.Bech32
  ( encodeLenient
  , dataPartFromBytes
  , dataPartToBytes
  , decodeLenient
  , humanReadablePartFromText
  )
```

* **Bech32 Encoding/Decoding:**
  Used to encode/decode Cardano addresses, crucial for interpreting and constructing addresses on-chain and off-chain.

### Data Types and Utilities

```haskell
import qualified Data.Text                  as T
import           Data.Bits                  ((.|.), (.&.), shiftL, shiftR)
import           Data.Word                  (Word8)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Base16     as B16
```

* **Text and ByteString:** Commonly used to handle binary and textual data.
* **Bitwise Operations:** For assembling and disassembling address headers.

### Plutus Ledger and Builtins

```haskell
import qualified Plutus.V1.Ledger.Crypto    as Crypto
import qualified PlutusTx.Builtins.Class    as Builtins
```

* **Crypto.PubKeyHash:** Represents a hashed public key used to authenticate transactions.
* **Builtins.toBuiltin:** Converts standard ByteStrings to Plutus on-chain ByteStrings.

---

## 3. Functions and Types Explained with Examples

### `bech32ToPubKeyHash`

Extracts a public key hash from a Shelley-style Bech32 address.

**Source Code:**

```haskell
bech32ToPubKeyHash :: String -> Either String Crypto.PubKeyHash
```

**Example:**

```haskell
bech32ToPubKeyHash "addr_test1vq0hjvh...xyz"
```

---

### Address Construction Functions

#### `pkhToAddrB32`

Constructs an enterprise-style Bech32 address from a given public key hash and network settings.

**Source Code:**

```haskell
pkhToAddrB32 :: String -> Word8 -> String -> Either String String
```

**Parameters:**

* HRP (human-readable part): "addr" for mainnet or "addr\_test" for testnet.
* Network identifier: `0` for testnet, `1` for mainnet.
* Public key hash in hexadecimal.

**Example:**

```haskell
pkhToAddrB32 "addr_test" 0 "659ad08ff173857842dc6f8bb0105253b9713d2e5e370ccb880d6d50"
```

---

#### `pkhToAddrB32Opt`

Similar to `pkhToAddrB32`, but with optional parameters for flexibility.

**Source Code:**

```haskell
pkhToAddrB32Opt :: Maybe String -> Maybe Word8 -> String -> Either String String
```

**Example:**

```haskell
-- Using defaults (addr_test, testnet)
pkhToAddrB32Opt Nothing Nothing "659ad08ff173857842dc6f8bb0105253b9713d2e5e370ccb880d6d50"

-- Specifying HRP explicitly
pkhToAddrB32Opt (Just "addr") (Just 1) "659ad08ff173857842dc6f8bb0105253b9713d2e5e370ccb880d6d50"
```

---

#### `pkhToAddrB32Testnet` & `pkhToAddrB32Mainnet`

Convenient wrappers for creating testnet/mainnet addresses.

**Example:**

```haskell
pkhToAddrB32Testnet "659ad08ff173857842dc6f8bb0105253b9713d2e5e370ccb880d6d50"

pkhToAddrB32Mainnet "659ad08ff173857842dc6f8bb0105253b9713d2e5e370ccb880d6d50"
```

---

### Address Decoding

#### `decodeBech32Address`

Decodes Shelley-style Bech32 addresses, extracting payment and staking credentials if present.

**Address Types Supported:**

* Enterprise address (payment credential only)
* Base address (payment and stake credentials)

**Data type returned:**

```haskell
data AddressInfo
  = EnterpriseAddr Crypto.PubKeyHash
  | BaseAddr Crypto.PubKeyHash BS.ByteString
  deriving Show
```

**Example:**

```haskell
decodeBech32Address "addr_test1vq0hjvh...xyz"
-- returns EnterpriseAddr or BaseAddr with credentials
```

---

### `rebuildBaseAddress`

Rebuilds a complete base address from payment and stake credential hashes.

**Source Code:**

```haskell
rebuildBaseAddress
  :: String
  -> Word8
  -> Word8
  -> BS.ByteString
  -> BS.ByteString
  -> Either String String
```

**Parameters Explained:**

* HRP ("addr" for mainnet, "addr\_test" for testnet)
* Address type (0 = key/key, 1 = key/script)
* Network identifier (0=testnet, 1=mainnet)
* Payment key hash (28 bytes)
* Stake credential hash (28 bytes)

**Example:**

```haskell
rebuildBaseAddress "addr_test" 0 0 payKeyHashBytes stakeKeyHashBytes
```

---

## 4. Practical Workflow Example

### Converting Hex Public Key Hash to Testnet Address:

```haskell
let pkhHex = "659ad08ff173857842dc6f8bb0105253b9713d2e5e370ccb880d6d50"
case pkhToAddrB32Testnet pkhHex of
  Left err -> putStrLn $ "Error: " ++ err
  Right addr -> putStrLn $ "Testnet address: " ++ addr
```

### Decoding a Bech32 Address to extract PKH:

```haskell
let address = "addr_test1vq0hjvh...xyz"
case decodeBech32Address address of
  Left err -> putStrLn $ "Error: " ++ err
  Right (EnterpriseAddr pkh) -> putStrLn $ "Payment PKH: " ++ show pkh
  Right (BaseAddr pkh stakeCred) -> do
    putStrLn $ "Payment PKH: " ++ show pkh
    putStrLn $ "Stake credential: " ++ show stakeCred
```

---

## 5. Best Practices Summary:

* Always validate the input lengths and formats when decoding addresses.
* Clearly handle both enterprise and base address scenarios in your code.
* Use convenience functions (`pkhToAddrB32Testnet`, `pkhToAddrB32Mainnet`) for common use-cases.

---

