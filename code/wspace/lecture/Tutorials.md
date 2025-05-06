## Table of Contents

1. [Vesting Module](#1-vesting-module)
2. [Parameterized Vesting Module](#2-parameterized-vesting-module)
3. [Time Utilities (CGTime)](#3-time-utilities-cgtime)
4. [Address Utilities (CGPlutusUtils)](#4-address-utilities-cgplutusutils)
5. [Glossary of Terms](#5-glossary-of-terms)

## 1. Vesting Module

The `Vesting` module implements a simple on-chain vesting contract that releases funds to a beneficiary only after a deadline.

### Key Functions

* `mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool`

  * Core validator logic: checks signature and deadline.

* `mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()`

  * Wraps `mkVestingValidator` for compilation.

* `validator :: Validator`

  * The compiled validator script.

* `saveVal :: IO ()`

  * Writes the validator to a `.plutus` file.

* `vestingAddressBech32 :: Network -> String`

  * Returns the on-chain address in Bech32 format.

* `printVestingDatumJSON :: PubKeyHash -> String -> IO ()`

  * Serializes a `VestingDatum` to JSON for testing.

### Example

```haskell
import Vesting
import Utilities (Network(..))

main :: IO ()
main = do
  -- Save the validator
  saveVal
  -- Print the address for testnet
  putStrLn $ vestingAddressBech32 Testnet
  -- Prepare a datum JSON
  let beneficiary = "659ad08ff..."
      deadlineISO = "2025-06-01T00:00:00Z"
  printVestingDatumJSON beneficiary deadlineISO
```

## 2. Parameterized Vesting Module

`ParameterizedVesting` generalizes `Vesting` by parameterizing the script with `VestingParams`.

### Key Functions

* `mkParameterizedVestingValidator :: VestingParams -> () -> () -> ScriptContext -> Bool`

  * Checks `txSignedBy` and deadline for given parameters.

* `mkWrappedParameterizedVestingValidator :: VestingParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()`

  * Wraps the parameterized validator for compilation.

* `validator :: VestingParams -> Validator`

  * Produces a validator for specific parameters.

* `fromHexPKH :: String -> PubKeyHash`

  * Converts a hex string to a `PubKeyHash`.

* `saveVal :: VestingParams -> IO ()`

  * Writes the parameterized validator to a file.

### Example

```haskell
import ParameterizedVesting

main :: IO ()
main = do
  let params = VestingParams
        { beneficiary = fromHexPKH "659ad08ff..."
        , deadline    = 1717406400 -- POSIXTime
        }
  saveVal params
  putStrLn "Parameterized vesting validator saved."
```

## 3. Time Utilities (CGTime)

The `CGTime` module provides conversions and helpers between `UTCTime`, `POSIXTime`, and ISO‑8601.

### Key Functions

* `utcToPOSIX :: UTCTime -> POSIXTime`
* `iso8601ToPOSIX :: String -> Maybe POSIXTime`
* `posixToUTC :: POSIXTime -> UTCTime`
* `posixToISO8601 :: POSIXTime -> String`
* `getUTCNow :: IO UTCTime`
* `getPOSIXNow :: IO POSIXTime`
* `getISO8601Now :: IO String`
* `getTimeTriple :: IO (UTCTime,POSIXTime,String)`
* `addSeconds :: NominalDiffTime -> UTCTime -> UTCTime`
* `addDaysUTC :: Integer -> UTCTime -> UTCTime`
* `diffSeconds :: UTCTime -> UTCTime -> NominalDiffTime`
* `addSecondsPOSIX :: NominalDiffTime -> POSIXTime -> POSIXTime`
* `addDaysPOSIX :: Integer -> POSIXTime -> POSIXTime`
* `diffSecondsPOSIX :: POSIXTime -> POSIXTime -> NominalDiffTime`
* `formatUTC :: String -> UTCTime -> String`
* `parseUTC :: String -> String -> Maybe UTCTime`
* `getLocalISO8601 :: IO String`

### Example

```haskell
import CGTime
import Data.Time.Clock (getCurrentTime)

main :: IO ()
main = do
  t <- getUTCNow
  print $ utcToPOSIX t
  print $ posixToISO8601 1714810800
  print =<< getISO8601Now
```

## 4. Address Utilities (CGPlutusUtils)

`CGPlutusUtils` lets you go between Bech32 addresses and `PubKeyHash`.

### Key Functions

* `bech32ToPubKeyHash :: String -> Either String PubKeyHash`
* `pkhToAddrB32 :: String -> Word8 -> String -> Either String String`
* `pkhToAddrB32Opt :: Maybe String -> Maybe Word8 -> String -> Either String String`
* `pkhToAddrB32Testnet :: String -> Either String String`
* `pkhToAddrB32Mainnet :: String -> Either String String`

### Example

```haskell
import CGPlutusUtils

main :: IO ()
main = do
  let hex = "659ad08ff173857842dc6f8..."
  case pkhToAddrB32Testnet hex of
    Left err   -> putStrLn $ "Error: " ++ err
    Right addr -> putStrLn $ "Address: " ++ addr
  print $ bech32ToPubKeyHash addr
```

## 5. Glossary of Terms

**Bech32**: A human-readable encoding for addresses.

**PubKeyHash**: A 28-byte hash of a public key used to identify wallets.

**POSIXTime**: Seconds since Unix epoch (1970‑01‑01 UTC).

**UTCTime**: Coordinated Universal Time representation in Haskell.

**HRP**: Human‑Readable Part of a Bech32 string, indicates network.

**GADT**: Generalized Algebraic Data Type, a Haskell feature for precise typing.

**Datum**: On-chain data attached to UTxOs.

**Validator**: A script that checks whether a transaction is allowed.

**On-chain**: Code that runs in the blockchain’s validation.

**Off-chain**: Code that runs in a user’s wallet or backend.

**CGTime**: Coxygen Global Time module

**CGPlutusUtils**: Coxygen Global Plutus Utils module

## 6. Test Suite

This section shows how to add and run Cabal tests for every module.

### 6.1 Configuring the Cabal File

Edit `wspace.cabal` to include a `test-suite` component:

```cabal
test-suite wspace-tests
  type:                exitcode-stdio-1.0
  hs-source-dirs:      tests
  main-is:             Spec.hs
  build-depends:
    base >=4.14 && <4.15,
    wspace,
    utilities,
    tasty,
    tasty-hunit,
    QuickCheck
  default-language:    Haskell2010
```

### 6.2 Example Test Files

#### 6.2.1 tests/VestingSpec.hs

```haskell
import Test.Tasty
import Test.Tasty.HUnit
import Vesting

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Vesting Module Tests"
  [ testCase "Validator rejects before deadline" $
      mkVestingValidator ... @?= False
  , testCase "Validator accepts after deadline" $
      mkVestingValidator ... @?= True
  ]
```

#### 6.2.2 tests/ParameterizedVestingSpec.hs

```haskell
import Test.Tasty
import Test.Tasty.HUnit
import ParameterizedVesting

main = defaultMain tests

tests = testGroup "Parameterized Vesting Tests"
  [ testCase "FromHexPKH parses valid hex" $
      fromHexPKH "659ad..." @?= expectedPKH
  ]
```

#### 6.2.3 tests/CGTimeSpec.hs

```haskell
import Test.Tasty
import Test.Tasty.HUnit
import CGTime

main = defaultMain tests

tests = testGroup "CGTime Tests"
  [ testCase "ISO8601 to POSIX roundtrip" $
      let tStr = "2025-05-04T00:00:00Z"
      in iso8601ToPOSIX tStr >>= 	 -> posixToISO8601 t @?= tStr
  ]
```

#### 6.2.4 tests/CGPlutusUtilsSpec.hs

```haskell
import Test.Tasty
import Test.Tasty.HUnit
import CGPlutusUtils

main = defaultMain tests

tests = testGroup "CGPlutusUtils Tests"
  [ testCase "Bech32 → PKH → Bech32 roundtrip" $
      let hex = "659ad08ff..."
      in do
        addr <- pkhToAddrB32Testnet hex
        Right pkh <- return $ bech32ToPubKeyHash addr
        pkh @?= Crypto.PubKeyHash (Builtins.toBuiltin (fst $ B16.decode (C.pack hex)))
  ]
```

### 6.3 Running Tests

```bash
cabal test
```

### 7 Additional Tutorials

# Comprehensive Plutus Smart Contracts Tutorial

This tutorial will provide a structured overview of your Plutus smart contract environment, specifically covering the modules, imports, functionalities, and testing approaches for both Plutus V1 and V2 smart contracts. It will also detail your `wspace.cabal` and `flake.nix` configurations to streamline development and testing.

## Module Breakdown

### 1. **On-chain Validator Modules**

#### **Vesting.hs** (Plutus V2)

* Defines `VestingDatum` with beneficiary and deadline.
* Validator logic: `mkVestingValidator`
* Helper functions to save compiled validator scripts and print datum in JSON.
* Critical imports:

  * `Plutus.V2.Ledger.Api` (core Plutus V2 functionalities)
  * `PlutusTx.Prelude` (on-chain compatible prelude functions)
  * `Utilities` (custom utilities like network address handling)

#### **ParameterizedVesting.hs** (Plutus V2)

* Defines `VestingParams` parameterized datum.
* Parameterized validator function: `mkParameterizedVestingValidator`
* Helper for handling public key hashes from hex strings.
* Important imports:

  * `Plutus.V2.Ledger.Api` for context handling
  * `PlutusTx` for compiling and lifting parameters

### 2. **Utilities and Helper Modules**

#### **CGTime.hs**

* Utilities for handling time conversions between `POSIXTime`, `UTCTime`, and ISO8601 strings.
* Important functions: `utcToPOSIX`, `iso8601ToPOSIX`, `posixToISO8601`
* Useful for deadline and time-based validations in contracts.

#### **CGPlutusUtilsv1.hs**

* Utilities for working with Bech32 addresses and public key hashes (`PubKeyHash`).
* Important for off-chain applications needing to decode and construct addresses.
* Key functions: `bech32ToPubKeyHash`, `pkhToAddrB32Testnet`, `decodeBech32Address`

### 3. **Testing Modules**

#### **Spec.hs**

* Aggregates all test suites (`VestingSpec`, `ParameterizedVestingSpec`, `CGTimeSpec`, `CGPlutusUtilsSpec`).
* Entry point for running tests with `defaultMain` from `tasty`.

#### **VestingSpec.hs**

* Unit tests validating the vesting logic with `dummyCtxBefore` and `dummyCtxAfter`.
* Demonstrates how to simulate contexts before and after deadlines.
* Uses modules from Plutus V2 (`Plutus.V2.Ledger.Api`, `Contexts`) and interval handling from Plutus V1.

#### **ParameterizedVestingSpec.hs**

* Tests for the parameterized vesting logic.
* Validates hex-to-`PubKeyHash` parsing functionality.

#### **CGTimeSpec.hs**

* Tests the correctness of time conversions (`iso8601ToPOSIX` and `posixToISO8601`).

#### **CGPlutusUtilsSpec.hs**

* Ensures correct encoding/decoding round-trip between Bech32 and `PubKeyHash`.

## Cabal and Nix Configurations

### **wspace.cabal**

* Structured into common blocks (`common-all`, `common-all-tests`) to manage dependencies effectively.
* Contains crucial dependencies:

  * `plutus-ledger-api`: Essential for Plutus smart contracts.
  * `plutus-simple-model`: Enables easy unit testing of Plutus scripts.
  * Testing dependencies (`tasty`, `QuickCheck`, `tasty-hunit`) for comprehensive test coverage.
* Compiler options ensure compatibility with Plutus contracts (`-fplugin-opt PlutusTx.Plugin:defer-errors`).

### **flake.nix**

* Defines a reproducible Nix development environment.
* Specifies GHC and Cabal versions matching Plutus contract requirements (typically GHC 8.10.7 for current Plutus tooling).
* Manages package sets and external dependencies ensuring consistent builds and CI compatibility.

## Handling Plutus V1 vs. V2

* **Plutus V1**: Commonly used modules like `Plutus.V1.Ledger.Interval` for interval handling.
* **Plutus V2**: Improved context (`ScriptContext` and `TxInfo`) offering reference inputs and redeemers. However, some utilities like interval handling remain in V1 modules.

### Best Practices:

* Clearly separate V1 and V2 imports to avoid ambiguity.
* Always initialize new fields introduced in Plutus V2 (`txInfoRedeemers`, `txInfoReferenceInputs`).
* Utilize `PlutusTx.AssocMap` for structured map fields (`txInfoData`, `txInfoWdrl`).

## Summary of Development and Testing Workflow

1. Define your validators clearly in separate modules (`Vesting.hs`, `ParameterizedVesting.hs`).
2. Leverage helper modules (`CGTime`, `CGPlutusUtilsv1`) for common, repetitive tasks.
3. Write robust unit tests (`Spec.hs` and related spec files) that simulate real-world on-chain contexts.
4. Maintain and clearly organize dependencies in `wspace.cabal`.
5. Use `flake.nix` to ensure a reproducible and predictable development environment.




