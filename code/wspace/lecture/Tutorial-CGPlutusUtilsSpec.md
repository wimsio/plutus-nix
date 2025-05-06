# Detailed Tutorial: Understanding and Using `CGPlutusUtilsSpec.hs`

This tutorial outlines the `CGPlutusUtilsSpec.hs` test file, explaining its imports, functionalities, and the specific testing methods utilized for verifying Bech32 and Public Key Hash (PKH) conversions, essential for managing addresses in Plutus smart contracts.

## 1. Imports Explanation

### Testing Libraries:

* **Test.Tasty**:

  * Provides structured test case organization (`testGroup`).
* **Test.Tasty.HUnit**:

  * Enables creation of unit tests with assertions (`testCase`, `assertFailure`, `@?=`).

### Utility Modules:

* **CGPlutusUtilsv1 (`pkhToAddrB32Testnet`, `bech32ToPubKeyHash`)**:

  * Custom utilities for converting between Bech32 addresses and Public Key Hashes specifically for testnet.

### Cryptographic and Encoding Libraries:

* **Data.ByteString.Base16 (B16)**:

  * For decoding hexadecimal strings to byte arrays.
* **Data.ByteString.Char8 (C)**:

  * Handling byte strings as ASCII characters.
* **Plutus.V1.Ledger.Crypto (Crypto)**:

  * Provides the cryptographic type `PubKeyHash`.
* **PlutusTx.Builtins.Class (Builtins)**:

  * Essential for converting byte arrays to Plutus built-in types.

## 2. Key Functionalities Explained

### `pkhToAddrB32Testnet`:

* Converts a Public Key Hash represented as a hexadecimal string into a Bech32 testnet address.
* Returns `Left` with an error message if conversion fails.

### `bech32ToPubKeyHash`:

* Converts a Bech32 address back to its corresponding `PubKeyHash`.
* Also returns `Left` with an error message if decoding fails.

## 3. Test Structure

The tests are organized clearly under:

```haskell
tests :: TestTree
tests = testGroup "CGPlutusUtils Tests"
```

### Test Case Explained:

* **"Bech32 ↔ PKH ↔ Bech32 round-trip"**:

  * Begins with a known hexadecimal representation of a Public Key Hash.
  * Attempts to convert this hex into a Bech32 address using `pkhToAddrB32Testnet`.
  * If this succeeds, it immediately converts the address back into a `PubKeyHash` using `bech32ToPubKeyHash`.
  * Finally, it decodes the original hex string directly into bytes and compares the derived `PubKeyHash` with the one directly constructed from these bytes.
  * Utilizes structured error handling and assertions (`assertFailure`, `@?=`) to ensure precise validation of each conversion step.

## 4. Extending Tests

To robustly verify functionality, consider the following additional test scenarios:

* **Invalid Hex Input:**

```haskell
  testCase "Invalid hex input to pkhToAddrB32Testnet" $
    let invalidHex = "invalidhex"
    in pkhToAddrB32Testnet invalidHex @?= Left "Expected error message"
```

* **Invalid Bech32 Input:**

```haskell
  testCase "Invalid Bech32 input to bech32ToPubKeyHash" $
    let invalidAddr = "invalidbech32"
    in bech32ToPubKeyHash invalidAddr @?= Left "Expected error message"
```

## 5. Best Practices

* Explicitly handle each possible error scenario.
* Clearly structure tests and handle failure conditions to aid debugging.
* Regularly run these tests after any changes to related conversion logic to ensure consistency and correctness.
