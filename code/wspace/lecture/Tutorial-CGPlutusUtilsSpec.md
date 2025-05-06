Here is your enhanced tutorial for `CGPlutusUtilsSpec.hs`, now fully structured with a **Table of Contents**, consistent formatting, and a clear **Glossary of Terms** — all while keeping the original content unchanged:

---

# 🧾 Detailed Tutorial: Understanding and Using `CGPlutusUtilsSpec.hs`

This tutorial outlines the `CGPlutusUtilsSpec.hs` test file, explaining its imports, functionalities, and the specific testing methods utilized for verifying Bech32 and Public Key Hash (PKH) conversions—essential for managing addresses in Plutus smart contracts.

---

## 📚 Table of Contents

1. [📦 Imports Explanation](#1-imports-explanation)
2. [🔧 Key Functionalities Explained](#2-key-functionalities-explained)
3. [🧪 Test Structure](#3-test-structure)
4. [➕ Extending Tests](#4-extending-tests)
5. [✅ Best Practices](#5-best-practices)
6. [📘 Glossary of Terms](#6-glossary-of-terms)

---

## 1. 📦 Imports Explanation

### Testing Libraries

* **Test.Tasty**
  Provides structured test case organization via `testGroup`.

* **Test.Tasty.HUnit**
  Enables creation of unit tests with assertions like `testCase`, `assertFailure`, and `@?=`.

### Utility Modules

* **CGPlutusUtilsv1 (`pkhToAddrB32Testnet`, `bech32ToPubKeyHash`)**
  Custom utilities for converting between Bech32 addresses and Public Key Hashes (PKHs), specifically for testnet usage.

### Cryptographic and Encoding Libraries

* **Data.ByteString.Base16 (B16)**
  For decoding hexadecimal strings to byte arrays.

* **Data.ByteString.Char8 (C)**
  Handles byte strings as ASCII characters.

* **Plutus.V1.Ledger.Crypto (Crypto)**
  Provides the cryptographic `PubKeyHash` type.

* **PlutusTx.Builtins.Class (Builtins)**
  Converts byte arrays to Plutus-compatible built-in types.

---

## 2. 🔧 Key Functionalities Explained

### `pkhToAddrB32Testnet`

* Converts a public key hash (in hexadecimal) into a Bech32-encoded testnet address.
* Returns `Left <error message>` upon failure.

---

### `bech32ToPubKeyHash`

* Converts a Bech32 Cardano address back into a `PubKeyHash`.
* Returns `Left <error message>` if decoding fails.

---

## 3. 🧪 Test Structure

### Test Group Declaration

```haskell
tests :: TestTree
tests = testGroup "CGPlutusUtils Tests"
```

### Main Test Case: **"Bech32 ↔ PKH ↔ Bech32 round-trip"**

* **Workflow:**

  1. Start with a known hex representation of a `PubKeyHash`.
  2. Convert it to a Bech32 testnet address using `pkhToAddrB32Testnet`.
  3. Convert that Bech32 address back to a `PubKeyHash` using `bech32ToPubKeyHash`.
  4. Independently construct the expected `PubKeyHash` from raw decoded bytes.
  5. Compare the round-trip output with the original using `@?=`.

* **Error Handling:**

  * Structured use of `assertFailure` when decoding or conversions fail.

---

## 4. ➕ Extending Tests

Here are suggested test cases to improve coverage and robustness:

### Invalid Hex Input

```haskell
testCase "Invalid hex input to pkhToAddrB32Testnet" $
  let invalidHex = "invalidhex"
  in pkhToAddrB32Testnet invalidHex @?= Left "Expected error message"
```

### Invalid Bech32 Input

```haskell
testCase "Invalid Bech32 input to bech32ToPubKeyHash" $
  let invalidAddr = "invalidbech32"
  in bech32ToPubKeyHash invalidAddr @?= Left "Expected error message"
```

These tests ensure that edge cases are properly handled and informative errors are raised.

---

## 5. ✅ Best Practices

* Handle every `Left`/`Right` case explicitly to prevent silent failures.
* Use descriptive assertion failure messages to aid debugging.
* Re-run this suite frequently when modifying address conversion logic.
* Group logically related tests for improved maintainability and clarity.

---

## 6. 📘 Glossary of Terms

| Term                      | Definition                                                               |
| ------------------------- | ------------------------------------------------------------------------ |
| **Bech32**                | A human-readable encoding format used for Cardano addresses.             |
| **PubKeyHash (PKH)**      | A hash of a public key, used to identify wallet credentials.             |
| **Hex String**            | A string representing binary data in base-16 (e.g., `"659ad08f..."`).    |
| **Testnet**               | A public Cardano blockchain for testing without real ADA.                |
| **`Either` Type**         | Represents success (`Right value`) or failure (`Left error`).            |
| **`@?=`**                 | Assertion that two values are equal in HUnit.                            |
| **`assertFailure`**       | Explicitly fails a test with a message.                                  |
| **Round-trip Conversion** | A test where an encoding followed by decoding yields the original input. |

---

