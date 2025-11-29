# 🛩️ Detailed Tutorial: Understanding and Using Aviation Maintenance Smart Contract

This tutorial covers an aviation maintenance and slot booking smart contract that manages aircraft parts, maintenance logs, and landing/takeoff fees on the Cardano blockchain.

---

## 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [🗃️ Data Structures](#2-data-structures)
3. [🧠 Core Validator Logic](#3-core-validator-logic)
4. [⚙️ Validator Script Compilation](#4-validator-script-compilation)
5. [🔧 Helper Functions](#5-helper-functions)
6. [🧪 Practical Usage Example](#6-practical-usage-example)
7. [🧷 Testing Strategy](#7-testing-strategy)
8. [✅ Best Practices](#8-best-practices)
9. [📘 Glossary of Terms](#9-glossary-of-terms)

---

## 1. 📦 Imports Overview

### Plutus Core Modules

* **Plutus.V2.Ledger.Api:**
  Provides fundamental types: `POSIXTime`, `PubKeyHash`, `Validator`, `ScriptContext`, `TxInfo`, and currency operations.

* **Plutus.V2.Ledger.Contexts:**
  Contains utility functions for transaction context validation (`txSignedBy`, `findOwnInput`).

* **Plutus.V1.Ledger.Interval:**
  Supplies interval checking functionality.

* **Plutus.V1.Ledger.Value:**
  Provides asset value operations (`valueOf`, `adaSymbol`, `adaToken`).

### Serialization & Cardano API

* **Codec.Serialise & ByteString:**
  Handle script serialization for on-chain deployment.

* **Cardano.Api:**
  Provides network-specific address generation and Bech32 encoding.

### PlutusTx Compilation

* **PlutusTx & PlutusTx.Prelude:**
  Enable script compilation, data serialization, and on-chain functions.

---

## 2. 🗃️ Data Structures

### Core Data Types

#### `PartNFT`
Represents a specific aircraft part with unique identification:
* `pnTailNo`: Aircraft tail number
* `pnSerialNo`: Part serial number  
* `pnRecordsHash`: Hash of maintenance records for integrity

#### `MaintenanceLog`
A single maintenance log entry:
* `mlWork`: Description of maintenance work performed
* `mlTimestamp`: When maintenance was completed
* `mlCertifier`: PubKeyHash of authorized maintenance certifier

#### `Slot`
Landing/takeoff slot booking:
* `slAirport`: Airport identifier
* `slTime`: Scheduled slot time
* `slFee`: Required fee in ADA

#### `AviationDatum`
Combined datum stored at script UTxO containing all relevant data:
* `adPartNFT`: Aircraft part NFT data
* `adLog`: Current maintenance log
* `adSlot`: Booking slot information
* `adOwner`: Aircraft owner's PubKeyHash

### Redeemer Type

#### `AviationAction`
Defines possible actions on the contract:
* `AppendLog`: Add new maintenance record
* `PaySlotFee`: Pay landing/takeoff fee

---

## 3. 🧠 Core Validator Logic

### `mkValidator`

**Purpose:** Ensures transactions follow aviation maintenance and fee payment rules.

#### AppendLog Validation:
* Must be signed by authorized certifier
* Verifies part NFT is present in transaction

#### PaySlotFee Validation:
* Must be signed by aircraft owner
* Ensures correct slot fee is paid to owner

### Helper Functions

#### `nftPresent`
Checks if the specific part NFT is present in the script input.

#### `paidTo`
Calculates total value paid to a specific public key hash in the transaction.

---

## 4. ⚙️ Validator Script Compilation

### `mkValidatorUntyped`
Wraps the typed validator for Plutus Core compatibility using `BuiltinData`.

### `validator`
Compiles the validator into a Plutus Core script ready for blockchain deployment.

---

## 5. 🔧 Helper Functions

### `plutusValidatorHash`
Generates the validator hash from the serialized script.

### `plutusScriptAddress`
Creates the script address for the validator.

### `toBech32ScriptAddress`
Generates human-readable Bech32 addresses for mainnet/testnet.

### `writeValidator`
Serializes and writes the validator to a `.plutus` file for deployment.

---

## 6. 🧪 Practical Usage Example

```haskell
-- Generate and deploy the validator
main

-- Expected output:
-- Validator written to: aviation-maintenance.plutus
-- 
-- --- Aviation Maintenance + Slot Fee Validator ---
-- Validator Hash (Plutus): ValidatorHash...
-- Plutus Script Address:    Address...
-- Bech32 Script Address:    addr_test...
-- -------------------------------------------------
-- Aviation validator generated successfully.
```

**Usage Scenarios:**

1. **Maintenance Recording:**
   - Certifier signs transaction with `AppendLog` redeemer
   - Adds new maintenance entry to log
   - Verifies part NFT ownership

2. **Slot Fee Payment:**
   - Owner signs transaction with `PaySlotFee` redeemer  
   - Pays required landing/takeoff fee
   - Maintains part NFT and logs

---

## 7. 🧷 Testing Strategy

* Test both successful and failing scenarios for each action
* Verify signature requirements with different key pairs
* Test fee payment validation with various amounts
* Validate NFT presence checks with different asset configurations

---

## 8. ✅ Best Practices

* Clearly define aircraft part identification using NFTs
* Maintain immutable maintenance records for audit trails
* Ensure proper authorization for maintenance and fee operations
* Test with realistic aviation industry scenarios
* Implement proper error messages for debugging

---

## 9. 📘 Glossary of Terms

| Term | Definition |
|------|------------|
| **Part NFT** | Non-fungible token representing a specific aircraft part with unique identification. |
| **Maintenance Log** | Immutable record of maintenance work performed on an aircraft part. |
| **Slot Booking** | Reserved time slot for aircraft landing or takeoff at an airport. |
| **Certifier** | Authorized entity that can sign off on maintenance work. |
| **POSIXTime** | Unix timestamp representation used for time-based validations. |
| **Bech32** | Human-readable address format used in Cardano. |
| **Validator Hash** | Unique identifier derived from the compiled validator script. |

---