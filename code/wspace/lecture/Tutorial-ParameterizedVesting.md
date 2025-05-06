# Tutorial: Understanding and Using `ParameterizedVesting.hs`

This tutorial explains the `ParameterizedVesting.hs` module in depth. You'll learn its structure, how to test it in `cabal repl`, and how it fits into a larger Plutus development workflow.

---

## 📑 Table of Contents

1. [Module Overview](#1-module-overview)
2. [Language Extensions](#2-language-extensions)
3. [Imports Overview](#3-imports-overview)
4. [Data Definitions](#4-data-definitions)
5. [Core Validator Logic](#5-core-validator-logic)
6. [Helper Functions](#6-helper-functions)
7. [Compiling and Saving the Script](#7-compiling-and-saving-the-script)
8. [Practical Example (Full Workflow)](#8-practical-example-full-workflow)
9. [Using `cabal repl` to Test and Interact](#9-using-cabal-repl-to-test-and-interact)
10. [Best Practices](#10-best-practices)
11. [Glossary](#11-glossary)

---

## 1. Module Overview

`ParameterizedVesting.hs` defines a Plutus smart contract that locks funds until a given deadline, allowing only a specific beneficiary to unlock them. Both `beneficiary` and `deadline` are passed as parameters.

---

## 2. Language Extensions

```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
```

* These extensions enable advanced Haskell features required for Plutus contract compilation and typing.

---

## 3. Imports Overview

### Plutus API Modules

```haskell
import Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash, ScriptContext(..), TxInfo(..), Validator, from, mkValidatorScript)
import Plutus.V2.Ledger.Contexts (txSignedBy)
import Plutus.V1.Ledger.Interval (contains)
```

These imports provide:

* Transaction validation context
* Time handling
* Signing verification
* Basic on-chain types

### Compilation Helpers

```haskell
import PlutusTx (applyCode, compile, liftCode, makeLift)
import PlutusTx.Prelude (Bool, traceIfFalse, ($), (&&), (.))
```

For writing Plutus code in Haskell and compiling to UPLC.

### Utility Functions

```haskell
import Utilities (wrapValidator, writeValidatorToFile, posixTimeFromIso8601)
```

Used for:

* Wrapping validators
* Writing `.plutus` files
* Parsing ISO8601 time

### PubKeyHash Conversion

```haskell
import Plutus.V1.Ledger.Crypto (PubKeyHash(..))
import qualified PlutusTx.Builtins.Class as Builtins
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Base16 as B16
```

Used to convert string/hex public keys into the correct on-chain format.

---

## 4. Data Definitions

### `VestingParams`

```haskell
data VestingParams = VestingParams
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    }
makeLift ''VestingParams
```

This struct-like type defines the two parameters that determine:

* Who can withdraw the funds
* When they can be withdrawn

---

## 5. Core Validator Logic

### `mkParameterizedVestingValidator`

```haskell
{-# INLINABLE mkParameterizedVestingValidator #-}
mkParameterizedVestingValidator :: VestingParams -> () -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator params _ _ ctx =
    traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
    traceIfFalse "deadline not reached" deadlineReached
  where
    info = scriptContextTxInfo ctx
    signedByBeneficiary = txSignedBy info $ beneficiary params
    deadlineReached = contains (from $ deadline params) $ txInfoValidRange info
```

Checks:

* That the transaction is signed by the beneficiary
* That the current slot is after the deadline

---

## 6. Helper Functions

### `fromHexPKH`

```haskell
fromHexPKH :: String -> PubKeyHash
fromHexPKH hex =
  case B16.decode (C.pack hex) of
    Right decoded -> PubKeyHash (Builtins.toBuiltin decoded)
    Left err      -> error ("Hex decoding failed: " ++ err)
```

Used to convert a hex public key hash string into a `PubKeyHash` for use in the script.

---

## 7. Compiling and Saving the Script

### `validator` and `saveVal`

```haskell
validator :: VestingParams -> Validator
validator params =
  mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode params)

saveVal :: VestingParams -> IO ()
saveVal = writeValidatorToFile "./assets/parameterized-vesting.plutus" . validator
```

Compiles and writes a `.plutus` script based on the given `VestingParams`.

---

## 8. Practical Example (Full Workflow)

```haskell
import ParameterizedVesting
import CGPlutusUtilsv1
import CGTime
import Plutus.V1.Ledger.Time (POSIXTime(..))

-- Convert Bech32 address to PubKeyHash
let Right pkh = bech32ToPubKeyHash "addr_test1qp..."

-- Convert POSIXTime from system time
dd' <- CGTime.getPOSIXNow
let deadline = POSIXTime (floor dd')

-- Create vesting parameters
let vp = VestingParams pkh deadline

-- Write the .plutus file
saveVal vp
```

---

## 9. Using `cabal repl` to Test and Interact

### Step-by-Step

1. Launch:

   ```bash
   cabal repl
   ```

2. Load all modules:

   ```haskell
   :load ParameterizedVesting CGPlutusUtilsv1 CGTime Vesting
   ```

3. Import if needed:

   ```haskell
   import CGPlutusUtilsv1
   import ParameterizedVesting
   import qualified CGTime
   import Plutus.V1.Ledger.Time (POSIXTime(..))
   ```

4. Create variables:

   ```haskell
   let Right pkh = bech32ToPubKeyHash "addr_test1qp..."
   dd' <- CGTime.getPOSIXNow
   let deadline = POSIXTime (floor dd')
   let vp = VestingParams pkh deadline
   ```

5. Save:

   ```haskell
   saveVal vp
   ```

---

## 10. Best Practices

* Use `Right pkh = ...` to unpack safely from `Either`
* Always convert system `POSIXTime` with `floor` into Plutus `POSIXTime`
* Use qualified imports to avoid name conflicts
* Compile often and test logic with unit tests (e.g., `ParameterizedVestingSpec.hs`)

---

## 11. Glossary

| Term            | Meaning                                                 |
| --------------- | ------------------------------------------------------- |
| `POSIXTime`     | Time format in seconds since UNIX epoch (used on-chain) |
| `PubKeyHash`    | Public key hash representing wallet ownership           |
| `Validator`     | A Plutus smart contract function                        |
| `ScriptContext` | Transaction metadata passed to a validator              |
| `txSignedBy`    | Checks if a specific wallet signed the transaction      |
| `contains`      | Verifies time intervals                                 |
| `:load`         | GHCi command to load modules                            |
| `saveVal`       | Custom function that saves a `.plutus` file             |

---

Let me know if you'd like this broken down into smaller reusable files or extended to cover `Vesting.hs` and related tests!
