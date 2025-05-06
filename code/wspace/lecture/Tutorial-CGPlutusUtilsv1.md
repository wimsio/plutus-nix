Certainly! Below is your tutorial for `CGPlutusUtilsv1.hs`, now **reformatted with a clear Table of Contents, improved section flow, and a complete Glossary of Terms**, while preserving all of your original content:

---

# 🧾 Detailed Tutorial: Understanding and Using `CGPlutusUtilsv1.hs`

This tutorial provides a thorough overview of the `CGPlutusUtilsv1.hs` module, highlighting its imports, key functionalities, practical use cases, and recommended best practices. The module specializes in manipulating and interpreting Cardano Bech32 addresses.

---

## 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [⚙️ Core Functions and Types](#2-core-functions-and-types)
3. [🧱 Data Type: `AddressInfo`](#3-data-type-addressinfo)
4. [🧪 Practical Usage Examples](#4-practical-usage-examples)
5. [🧷 Testing Strategy](#5-testing-strategy)
6. [✅ Best Practices](#6-best-practices)
7. [📘 Language Extensions](#7-language-extensions)
8. [🔍 Imports and Their Usage](#8-imports-and-their-usage)
9. [📖 Function Definitions and Examples](#9-function-definitions-and-examples)
10. [🛠️ Practical Workflow Examples](#10-practical-workflow-examples)
11. [📘 Glossary of Terms](#11-glossary-of-terms)

---

## 1. 📦 Imports Overview

### Bech32 Encoding and Decoding

* **Codec.Binary.Bech32**
  Functions for encoding, decoding, and manipulating Bech32 addresses.

### Data Manipulation and Conversion

* **Data.Text**
  Essential for text processing.
* **Data.ByteString and Data.ByteString.Base16**
  Handle binary data manipulation and hexadecimal encoding/decoding.
* **Data.Bits and Data.Word**
  For bitwise operations required in parsing addresses.

### Crypto and Builtin Types

* **Plutus.V1.Ledger.Crypto**
  Provides cryptographic types like `PubKeyHash`.
* **PlutusTx.Builtins.Class**
  Facilitates conversion to Plutus built-in types.

---

## 2. ⚙️ Core Functions and Types

### Address Extraction and Conversion

* **`bech32ToPubKeyHash`**
  Decodes a Shelley-style Bech32 address to its payment `PubKeyHash`.

* **`decodeBech32Address`**
  Decodes addresses, distinguishing between enterprise and base addresses.

### Address Construction

* **`pkhToAddrB32`**
  Constructs a Bech32 enterprise address from a `PubKeyHash`.

* **`pkhToAddrB32Opt`**
  Constructs a Bech32 address with optional parameters (HRP and network ID).

* **`pkhToAddrB32Testnet` / `pkhToAddrB32Mainnet`**
  Convenience functions for testnet or mainnet address creation.

* **`rebuildBaseAddress`**
  Reconstructs a base address from payment and stake credentials.

---

## 3. 🧱 Data Type: `AddressInfo`

* Represents parsed address information.
* Distinguishes between:

  * **EnterpriseAddr**: Payment credential only
  * **BaseAddr**: Both payment and stake credentials

---

## 4. 🧪 Practical Usage Examples

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

---

## 5. 🧷 Testing Strategy

* Utilize rigorous unit testing for address decoding and construction scenarios.
* Validate round-trip conversions of addresses to ensure fidelity.
* Regularly verify handling of incorrect inputs to ensure error handling robustness.

---

## 6. ✅ Best Practices

* Always handle potential errors explicitly and provide meaningful messages.
* Document clearly any supported or unsupported address formats.
* Conduct comprehensive testing to prevent regressions and maintain robustness.

---

## 7. 📘 Language Extensions

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

* Enables string literals to flexibly work with `Text` or `ByteString`.

---

## 8. 🔍 Imports and Their Usage

### Bech32

```haskell
import Codec.Binary.Bech32 (...)
```

* Used for encoding and decoding Shelley addresses.

### Data Types & Utilities

```haskell
import qualified Data.Text as T
import           Data.Bits ((.|.), (.&.), shiftL, shiftR)
import           Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
```

* Manages binary manipulation and string conversion.

### Plutus Integration

```haskell
import qualified Plutus.V1.Ledger.Crypto as Crypto
import qualified PlutusTx.Builtins.Class as Builtins
```

* Bridges custom utilities with Plutus ledger types.

---

## 9. 📖 Function Definitions and Examples

### `bech32ToPubKeyHash`

```haskell
bech32ToPubKeyHash :: String -> Either String Crypto.PubKeyHash
```

* Converts a valid Shelley Bech32 address into a `PubKeyHash`.

---

### `pkhToAddrB32`

```haskell
pkhToAddrB32 :: String -> Word8 -> String -> Either String String
```

* Example:

```haskell
pkhToAddrB32 "addr_test" 0 "659ad08ff1..."
```

---

### `pkhToAddrB32Opt`

```haskell
pkhToAddrB32Opt :: Maybe String -> Maybe Word8 -> String -> Either String String
```

* Flexible construction with optional HRP and network ID.

---

### `pkhToAddrB32Testnet` & `pkhToAddrB32Mainnet`

* Wrapper functions for simplified address creation on testnet and mainnet.

---

### `decodeBech32Address`

* Returns `EnterpriseAddr` or `BaseAddr` with credential breakdown.

---

### `rebuildBaseAddress`

```haskell
rebuildBaseAddress
  :: String -> Word8 -> Word8 -> BS.ByteString -> BS.ByteString -> Either String String
```

* Builds a full base address using raw credential bytes.

---

## 10. 🛠️ Practical Workflow Examples

### Convert Hex PKH to Testnet Address

```haskell
let pkhHex = "659ad08ff173857842..."
case pkhToAddrB32Testnet pkhHex of
  Left err -> putStrLn $ "Error: " ++ err
  Right addr -> putStrLn $ "Testnet address: " ++ addr
```

---

### Decode Bech32 Address

```haskell
let address = "addr_test1vq0hjvh..."
case decodeBech32Address address of
  Left err -> putStrLn $ "Error: " ++ err
  Right (EnterpriseAddr pkh) -> print pkh
  Right (BaseAddr pkh stake) -> do
    print pkh
    print stake
```

---

## 11. 📘 Glossary of Terms

| Term                          | Definition                                                        |
| ----------------------------- | ----------------------------------------------------------------- |
| **Bech32**                    | Human-readable address encoding used in Cardano.                  |
| **Enterprise Address**        | Contains only payment credential.                                 |
| **Base Address**              | Contains both payment and staking credentials.                    |
| **PubKeyHash (PKH)**          | Hash of a public key; used to identify wallets.                   |
| **HRP (Human-Readable Part)** | Prefix in a Bech32 address (e.g., `addr`, `addr_test`).           |
| **Shelley Address**           | A Bech32-format address introduced in the Shelley era of Cardano. |
| **Plutus Builtin Types**      | Low-level types used in on-chain Plutus smart contracts.          |
| **ByteString**                | Binary string representation used for hashes and credentials.     |
| **Stake Credential**          | Optional part of a base address, enables staking rewards.         |
| **Word8**                     | 8-bit unsigned integer used for address flags and network IDs.    |

---

