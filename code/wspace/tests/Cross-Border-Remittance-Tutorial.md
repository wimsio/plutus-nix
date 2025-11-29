# 🌍 Detailed Tutorial: Understanding and Using Cross-Border Remittance Smart Contract

This tutorial covers a decentralized cross-border payment system with FX rate oracles and KYC verification on the Cardano blockchain.

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
  Provides fundamental types: `POSIXTime`, `PubKeyHash`, `TxOutRef`, `Value`.

* **Plutus.V2.Ledger.Contexts:**
  Contains utility functions for transaction context validation.

* **Plutus.V1.Ledger.Value:**
  Provides asset value operations (`valueOf`).

### Utility Modules

* **PlutusTx:**
  Enables script compilation and data serialization.

* **PlutusTx.Prelude:**
  Basic Plutus scripting functions.

* **PlutusTx.Builtins:**
  Provides built-in functions for byte string operations.

---

## 2. 🗃️ Data Structures

### `RemitDatum`

Defines the remittance's on-chain state:

* `rdSender`: Sender's public key hash
* `rdReceiver`: Receiver's public key hash
* `rdAmount`: Base amount to send
* `rdCorridor`: Payment corridor (e.g., "USD-ETH")
* `rdFxOracleRef`: FX rate oracle reference
* `rdKycRef`: KYC verification reference

### `RemitAction`

Defines possible remittance operations:
* `Deposit`: Sender deposits funds
* `Withdraw`: Receiver withdraws converted funds

---

## 3. 🧠 Core Validator Logic

### `mkValidator`

**Purpose:** Facilitates secure cross-border payments with FX conversion and compliance checks.

**Validation Conditions:**

#### Deposit (Sender):
* Transaction signed by sender
* FX rate oracle input must be present
* KYC reference input must be present
* Sender deposits exactly `rdAmount` in ADA

#### Withdraw (Receiver):
* Transaction signed by receiver
* FX rate oracle input must be present
* KYC reference input must be present
* Receiver receives amount adjusted by oracle rate

### Helper Functions

#### `hasRef`
Verifies specific transaction input references exist.

#### `valuePaidToPKH`
Calculates total value sent to a specific public key hash.

#### Oracle Functions
* `readOracleRate`: Extracts exchange rate from oracle datum
* `getOracleOutput`: Retrieves oracle output from transaction inputs

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
-- Validator written to: cross-border-remit.plutus
-- 
-- --- Cross-Border Remittance Validator Info ---
-- Validator Hash (Plutus): ValidatorHash...
-- Plutus Script Address:    Address...
-- Bech32 Script Address:    addr_test...
-- ------------------------------------------------
-- Cross-border remittance validator generated.
```

**Remittance Flow:**

1. **Deposit Phase:**
   - Sender locks funds with FX oracle and KYC verification
   - All references must be present

2. **Withdrawal Phase:**
   - Receiver claims converted amount using current FX rate
   - Proper authorization and reference verification

---

## 7. 🧷 Testing Strategy

* Test with various FX rate scenarios
* Verify KYC reference validation
* Test authorization requirements for both parties
* Validate FX conversion calculations

---

## 8. ✅ Best Practices

* Use trusted FX rate oracles with proven track records
* Implement robust KYC/AML compliance procedures
* Set appropriate minimum amounts for cost efficiency
* Include fallback mechanisms for oracle failures
* Test with multiple currency corridors

---

## 9. 📘 Glossary of Terms

| Term | Definition |
|------|-------------|
| **Remittance** | Cross-border payment transfer |
| **Corridor** | Currency pair for exchange |
| **FX Oracle** | Trusted exchange rate data source |
| **KYC Reference** | Know Your Customer verification proof |
| **Scaled Rate** | Exchange rate with decimal precision |

---