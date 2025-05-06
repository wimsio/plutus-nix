## Detailed Tutorial: Understanding and Using `ParameterizedVesting.hs`

This tutorial provides a detailed explanation of the `ParameterizedVesting.hs` module, outlining its purpose, imports, core functionalities, and practical applications in Plutus smart contracts.

## 1. Imports Overview

### Plutus API Modules:

* **Plutus.V2.Ledger.Api:**

  * Essential Plutus types such as `POSIXTime`, `PubKeyHash`, `ScriptContext`, `TxInfo`, and utilities for validator scripts.
* **Plutus.V2.Ledger.Contexts:**

  * Utilities for context inspection like `txSignedBy`.
* **Plutus.V1.Ledger.Interval:**

  * Provides the `contains` function to check time intervals.

### PlutusTx and Builtin Operations:

* **PlutusTx:**

  * Enables compilation of Haskell code into Plutus Core.
* **PlutusTx.Prelude:**

  * Offers basic functions and types for Plutus on-chain code.

### Utility Imports:

* **Utilities:**

  * Custom utility functions such as `wrapValidator`, `writeValidatorToFile`, and time conversion functions.
* **Data.ByteString and Data.ByteString.Base16:**

  * ByteString handling and hexadecimal encoding/decoding.

## 2. Data Structures

### VestingParams

* Holds parameters required by the validator:

  * `beneficiary`: The public key hash that must sign transactions.
  * `deadline`: The POSIX timestamp after which funds can be spent.

## 3. Core Validator Logic

### `mkParameterizedVestingValidator`

* **Purpose:**

  * Ensures transactions meet two conditions:

    * Signed by the specified beneficiary.
    * Occurs after a certain deadline.

* **Inputs:**

  * `VestingParams`: Contains beneficiary and deadline information.
  * Script inputs are not utilized (`()` placeholders).
  * `ScriptContext`: Provides transaction context information.

* **Validation Checks:**

  * **Signed by Beneficiary:** Checks if the transaction is signed by the intended beneficiary.
  * **Deadline Check:** Verifies the current transaction is within a valid time interval after the deadline.

### `mkWrappedParameterizedVestingValidator`

* Wraps the core validator for compatibility with Plutus on-chain code (`BuiltinData`).

## 4. Helper Functions

### `fromHexPKH`

* Converts a hexadecimal string representation to a `PubKeyHash`. Useful for setting beneficiary keys.

### `saveVal`

* Writes the compiled validator script to a `.plutus` file for deployment.

## 5. Practical Usage Example

```haskell
-- Define VestingParams
let params = VestingParams
              { beneficiary = fromHexPKH "659ad08ff1..."
              , deadline = posixTimeFromIso8601 "2025-05-04T14:30:00Z"
              }

-- Save validator script
saveVal params
```

## 6. Testing Strategy

* Verify validator logic with scenarios both before and after the deadline.
* Ensure proper signature checking by simulating transactions signed and not signed by the beneficiary.

## 7. Best Practices

* Clearly define and handle parameters and edge cases explicitly.
* Utilize meaningful error messages with `traceIfFalse` for debugging and clarity.
* Regularly test your validator scripts with diverse scenarios to maintain correctness and reliability.

## 8. Summary

Here's a detailed explanation of each key component in the provided Haskell Plutus smart contract module, complete with clear source code examples.

---

## Explanation of Key Components in `ParameterizedVesting.hs`

This module implements a parameterized vesting contract on Cardano’s blockchain using Plutus. Let’s explore each important part step-by-step with clear examples.

---

## 1. Language Extensions

```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
```

* **DataKinds:** Enables type-level literals and promotes data constructors to types.
* **MultiParamTypeClasses:** Allows type classes with more than one parameter.
* **NoImplicitPrelude:** Disables the standard Prelude to use Plutus-specific Prelude.
* **OverloadedStrings:** Simplifies working with string-like data.
* **ScopedTypeVariables:** Allows explicit type annotations inside scoped blocks.
* **TemplateHaskell:** Allows embedding compile-time Haskell code generation.

---

## 2. Imports

### Plutus API and Contexts

* **Interval checks:**

```haskell
import Plutus.V1.Ledger.Interval (contains)
```

Used for checking if a given interval contains a timestamp.

* **Core types and functions:**

```haskell
import Plutus.V2.Ledger.Api
  ( BuiltinData, POSIXTime, PubKeyHash
  , ScriptContext (scriptContextTxInfo)
  , TxInfo (txInfoValidRange)
  , Validator, from, mkValidatorScript
  )
```

Plutus V2 types for representing transaction information and validation contexts.

* **Context utilities:**

```haskell
import Plutus.V2.Ledger.Contexts (txSignedBy)
```

Checks if transactions are signed by a specific public key hash.

---

### PlutusTx and Built-in Helpers

```haskell
import PlutusTx (applyCode, compile, liftCode, makeLift)
import PlutusTx.Prelude (Bool, traceIfFalse, ($), (&&), (.))
```

* **compile:** Compiles Haskell functions to Plutus on-chain code.
* **makeLift:** Automatically generates code to lift custom types into on-chain code.
* **traceIfFalse:** Emits debugging messages in transaction failures.

---

### Utility functions

```haskell
import Utilities (wrapValidator, writeValidatorToFile, posixTimeFromIso8601)
```

* **wrapValidator:** Converts typed validator functions into a standard Plutus form.
* **writeValidatorToFile:** Writes compiled validator scripts to file.
* **posixTimeFromIso8601:** Parses ISO8601-formatted strings to POSIX timestamps.

---

## 3. Custom Data Types

### VestingParams

```haskell
data VestingParams = VestingParams
  { beneficiary :: PubKeyHash
  , deadline    :: POSIXTime
  }
makeLift ''VestingParams
```

* Defines the contract parameters clearly and concisely.
* `makeLift` generates the necessary instances to use this type on-chain.

---

## 4. On-Chain Validation Logic

### mkParameterizedVestingValidator

```haskell
{-# INLINABLE mkParameterizedVestingValidator #-}
mkParameterizedVestingValidator :: VestingParams -> () -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator params () () ctx =
    traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
    traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ beneficiary params

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline params) $ txInfoValidRange info
```

**Explanation:**
This validator ensures two conditions for spending the funds:

* **Signature Check:** The transaction must be signed by the beneficiary.
* **Deadline Check:** The current time (`txInfoValidRange`) must be past the specified deadline.

**Example usage of validation logic:**

```haskell
params = VestingParams beneficiaryPKH deadlineTimestamp
mkParameterizedVestingValidator params () () transactionContext
```

---

### Wrapped Validator

```haskell
{-# INLINABLE mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator :: VestingParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator = wrapValidator . mkParameterizedVestingValidator

validator :: VestingParams -> Validator
validator params = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode params)
```

* Wraps the typed validator into the format expected by Plutus Core scripts.
* Compiles the validator using Template Haskell and PlutusTx for on-chain use.

---

## 5. Helper Functions

### fromHexPKH

```haskell
fromHexPKH :: String -> PubKeyHash
fromHexPKH hex =
  case B16.decode (C.pack hex) of
    Right decoded -> PubKeyHash (Builtins.toBuiltin decoded)
    Left err      -> error ("Hex decoding failed: " ++ err)
```

* Converts a hex string representation of a public key hash into a usable `PubKeyHash` type.

**Example:**

```haskell
fromHexPKH "659ad08ff173857842dc6f8bb0105253b9713d2e5e370ccb880d6d50"
```

---

### saveVal

```haskell
saveVal :: VestingParams -> IO ()
saveVal = writeValidatorToFile "./assets/parameterized-vesting.plutus" . validator
```

* Saves compiled validator scripts to a file, ready for deployment.

**Example:**

```haskell
let params = VestingParams
              (fromHexPKH "659ad08ff173857842dc6f8bb0105253b9713d2e5e370ccb880d6d50")
              (posixTimeFromIso8601 "2025-05-04T00:00:00Z")

saveVal params
```

---

## 6. Practical Example (Full Workflow)

```haskell
-- Define VestingParams with a beneficiary and deadline
let beneficiaryHex = "659ad08ff173857842dc6f8bb0105253b9713d2e5e370ccb880d6d50"
let beneficiaryPKH = fromHexPKH beneficiaryHex
let deadline = posixTimeFromIso8601 "2025-05-04T00:00:00Z"

let params = VestingParams beneficiaryPKH deadline

-- Save compiled validator
saveVal params
```

---

## Summary of Workflow:

1. **Define Parameters:** Clearly define beneficiary public key hash and deadline.
2. **Validation Logic:** Implement conditions within `mkParameterizedVestingValidator`.
3. **Compile and Save:** Compile validator using PlutusTx and save to a `.plutus` file.
4. **Deploy:** Deploy the script to the Cardano blockchain and interact with it via off-chain code.

---

## 9. Tutorial: Using `import`, `:load`, and Testing `VestingParams` in `cabal repl`

This tutorial walks you through how to:
- Load multiple modules in `cabal repl`
- Import and access functions across modules
- Correctly define `VestingParams` using `PubKeyHash` and `POSIXTime`
- Serialize the validator to a `.plutus` file

---

## 1. Launching `cabal repl`

Navigate to your project directory:
```bash
cd ~/plutus-nix/code
cabal repl
```

---

## 2. Loading Multiple Modules

To load multiple modules:
```haskell
:load ParameterizedVesting CGPlutusUtilsv1 CGTime Vesting
```

Now GHCi loads all four modules for use.

---

## 3. Importing Functions After Load

If some functions still aren't recognized, you can import the modules explicitly:
```haskell
import ParameterizedVesting
import CGPlutusUtilsv1
import CGTime
import Vesting
```

If you want to use a specific name and avoid conflicts:
```haskell
import qualified CGTime
```

---

## 4. Creating a `PubKeyHash` from a Bech32 Address

Use the Bech32 testnet address and extract the public key hash:
```haskell
let Right pkh = bech32ToPubKeyHash "addr_test1qp..."
```

Make sure you bind with `Right pkh =` to safely unwrap `Either`.

---

## 5. Getting the Deadline as `POSIXTime`

The function `getPOSIXNow` from `CGTime` gives the current system time.
However, its result must be converted to Plutus-compatible `POSIXTime`.

```haskell
import Plutus.V1.Ledger.Time (POSIXTime(..))
dd' <- CGTime.getPOSIXNow
let deadline = POSIXTime (floor dd')
```

---

## 6. Creating Vesting Parameters

Now create the `VestingParams`:

```haskell
let vp = VestingParams pkh deadline
```

You can inspect the object using `:t vp` or `print vp`.

---

## 7. Saving the Plutus Script

Use `saveVal` to serialize the contract:
```haskell
saveVal vp
```

This writes the compiled script to:
```text
./assets/parameterized-vesting.plutus
```

---

## Troubleshooting

- **Module Not in Scope**: Use `:load` to bring it in or add to `.cabal` dependencies.
- **Type Mismatch**: Use `POSIXTime (floor time)` to convert from system types.
- **Name Shadowing**: Rename conflicting variables or use qualified imports.

---

With this workflow, you can reliably load, test, and serialize parameterized Plutus contracts in GHCi.


