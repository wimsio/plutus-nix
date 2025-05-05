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

