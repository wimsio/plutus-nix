# 🔄 Detailed Tutorial: Understanding and Using NFT Escrow Smart Contract

This tutorial covers a secure NFT-based escrow system for trustless trading with time-based refunds on the Cardano blockchain.

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
  Provides fundamental types: `POSIXTime`, `PubKeyHash`, `CurrencySymbol`, `TokenName`.

* **Plutus.V2.Ledger.Contexts:**
  Contains utility functions for transaction context validation.

* **Plutus.V1.Ledger.Interval:**
  Supplies interval functionality for time-based operations.

* **Plutus.V1.Ledger.Value:**
  Provides asset value operations for NFT verification.

### Utility Modules

* **PlutusTx:**
  Enables script compilation and data serialization.

* **PlutusTx.Prelude:**
  Basic Plutus scripting functions.

* **PlutusTx.Builtins:**
  Provides built-in functions for Plutus operations.

---

## 2. 🗃️ Data Structures

### `EscrowDatum`

Defines the escrow's on-chain state:

* `edBuyer`: Buyer's public key hash
* `edSeller`: Seller's public key hash
* `edAmount`: Purchase price in Lovelace
* `edDeadline`: Refund deadline timestamp
* `edCurrency`: NFT currency symbol
* `edToken`: NFT token name

### `EscrowAction`

Defines possible escrow operations:
* `PaySeller`: Complete purchase and pay seller
* `RefundSeller`: Refund NFT to seller after deadline

---

## 3. 🧠 Core Validator Logic

### `mkValidator`

**Purpose:** Ensures secure NFT trading with buyer and seller protection.

**Validation Conditions:**

#### PaySeller (Complete Purchase):
* Escrow must contain the specified NFT
* Transaction signed by buyer
* Seller receives full `edAmount` in ADA
* Buyer receives the NFT from escrow

#### RefundSeller (Cancel Transaction):
* Escrow must contain the specified NFT
* Transaction signed by seller
* Current time must be after `edDeadline`
* Seller receives their NFT back

### Helper Functions

#### `scriptInputContainsNFT`
Verifies the escrow UTxO contains the specified NFT.

#### `afterDeadline`
Checks if current transaction time is after the refund deadline.

---

## 4. ⚙️ Validator Script Compilation

### `mkValidatorUntyped`
Wraps the typed validator for Plutus Core compatibility.

### `validator`
Compiles the validator into a Plutus Core script ready for deployment.

---

## 5. 🔧 Helper Functions

### `plutusValidatorHash`
Generates the validator hash from the serialized script.

### `plutusScriptAddress`
Creates the script address for the validator.

### `toBech32ScriptAddress`
Generates human-readable Bech32 addresses.

### `writeValidator`
Serializes and writes the validator to a `.plutus` file.

---

## 6. 🧪 Practical Usage Example

```haskell
-- Generate and deploy the validator
main

-- Expected output:
-- Validator written to: validator.plutus
-- 
-- --- Escrow NFT Validator Info ---
-- Validator Hash (Plutus): ValidatorHash...
-- Plutus Script Address:    Address...
-- Bech32 Script Address:    addr_test...
-- ---------------------------------
-- Escrow NFT validator generated successfully.
```

**Escrow Operations:**

1. **Complete Purchase:**
   - Buyer pays agreed price to seller
   - NFT transferred from escrow to buyer
   - Requires buyer authorization

2. **Cancel & Refund:**
   - NFT returned to original seller
   - Only possible after deadline passes
   - Requires seller authorization

---

## 7. 🧷 Testing Strategy

* Test successful purchase completion
* Verify refund functionality after deadline
* Test authorization requirements for both parties
* Validate NFT presence checks in escrow

---

## 8. ✅ Best Practices

* Set reasonable deadlines based on trade complexity
* Use unique NFTs with proper metadata
* Implement clear dispute resolution procedures
* Test with various NFT types and values
* Include proper error handling for failed transactions

---

## 9. 📘 Glossary of Terms

| Term | Definition |
|------|-------------|
| **Escrow** | Neutral third party holding assets until conditions met |
| **NFT** | Non-Fungible Token representing unique digital asset |
| **CurrencySymbol** | Unique identifier for token policy |
| **TokenName** | Specific token identifier within policy |
| **POSIXTime** | Unix timestamp for deadline management |

---