# Detailed Tutorial: Understanding and Using `Vesting.hs`

This tutorial covers the `Vesting.hs` module, highlighting its purpose, critical imports, validator logic, and practical usage scenarios. This module is essential for creating simple vesting smart contracts in Plutus.

## 1. Imports Overview

### Plutus API Modules:

* **Plutus.V2.Ledger.Api:**

  * Provides fundamental types such as `POSIXTime`, `PubKeyHash`, and transaction context (`ScriptContext`, `TxInfo`).
* **Plutus.V2.Ledger.Contexts:**

  * Contains utility functions for transaction context validation (`txSignedBy`).
* **Plutus.V1.Ledger.Interval:**

  * Supplies the `contains` function for interval checking.

### Utility and Prelude Modules:

* **Utilities:**

  * Custom utility functions for validator scripts and JSON output.
* **PlutusTx:**

  * Enables script compilation and data serialization.
* **PlutusTx.Prelude:**

  * Basic Plutus scripting functions.
* **Prelude and Data.Maybe:**

  * Haskell standard library utilities for basic operations and handling optional values.

## 2. Data Structures

### `VestingDatum`

* Defines the contract's datum, which includes:

  * `beneficiary`: The public key hash required to authorize spending.
  * `deadline`: POSIX timestamp after which funds can be accessed.

## 3. Core Validator Logic

### `mkVestingValidator`

* **Purpose:**

  * Ensures transactions satisfy two conditions:

    * Must be signed by the designated beneficiary.
    * Must occur after the specified deadline.

* **Validation Conditions:**

  * Checks for beneficiary's signature using `txSignedBy`.
  * Verifies the transaction time interval against the deadline.

### `mkWrappedVestingValidator`

* Wraps the core validator function for compatibility with Plutus on-chain scripts using `BuiltinData`.

## 4. Validator Script Compilation

### `validator`

* Compiles the validator function into a Plutus Core script ready for blockchain deployment.

## 5. Helper Functions

### `saveVal`

* Writes the compiled validator script to a `.plutus` file for deployment.

### `vestingAddressBech32`

* Generates the validator script’s Bech32 address for given network (mainnet/testnet).

### `printVestingDatumJSON`

* Outputs the datum required by the validator in JSON format for use in off-chain applications.

## 6. Practical Usage Example

```haskell
-- Save the compiled validator script
saveVal

-- Print the Bech32 address for testnet
putStrLn $ vestingAddressBech32 Testnet

-- Generate VestingDatum in JSON format
printVestingDatumJSON (fromHexPKH "659ad08ff1...") "2025-05-04T14:30:00Z"
```

## 7. Testing Strategy

* Validate the contract with scenarios around the deadline boundary.
* Confirm signature checks by simulating both successful and failing beneficiary signatures.

## 8. Best Practices

* Always clearly define and handle critical datum fields.
* Provide meaningful trace messages for easier debugging.
* Frequently test your smart contracts with diverse and edge-case scenarios to maintain accuracy and security.
