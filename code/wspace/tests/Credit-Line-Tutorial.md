# 💳 Detailed Tutorial: Understanding and Using Revolving Credit Line Smart Contract

This tutorial covers a flexible credit line system for decentralized revolving credit with dynamic limits and interest accrual on the Cardano blockchain.

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
  Provides fundamental types: `POSIXTime`, `PubKeyHash`, `Validator`, `ScriptContext`.

* **Plutus.V2.Ledger.Contexts:**
  Contains utility functions for transaction context validation.

* **Plutus.V1.Ledger.Interval:**
  Supplies interval functionality for time-based operations.

* **Plutus.V1.Ledger.Value:**
  Provides asset value operations.

### Utility Modules

* **PlutusTx:**
  Enables script compilation and data serialization.

* **PlutusTx.Prelude:**
  Basic Plutus scripting functions.

* **PlutusTx.Builtins:**
  Provides built-in functions for Plutus operations.

---

## 2. 🗃️ Data Structures

### `LineDatum`

Defines the credit line's on-chain state:

* `ldBorrower`: Borrower's public key hash
* `ldLimit`: Maximum credit limit
* `ldDrawn`: Currently drawn amount
* `ldRateModel`: Interest rate model parameters
* `ldLastAccrual`: Last interest accrual timestamp

### `LineAction`

Defines possible credit line operations:
* `Draw Integer`: Draw funds from credit line
* `Repay Integer`: Repay outstanding balance
* `AdjustLimit Integer`: Adjust credit limit (controller only)
* `Accrue`: Apply interest to drawn amount

---

## 3. 🧠 Core Validator Logic

### `mkValidator`

**Purpose:** Manages revolving credit operations with interest accrual and limit enforcement.

**Validation Conditions:**

#### Draw Funds:
* Ensures new draw doesn't exceed credit limit: `(drawn + amount) ≤ limit`
* Requires borrower's signature for authorization

#### Repay Balance:
* Prevents over-repayment: `amount ≤ drawn`
* Requires borrower's signature for authorization

#### Adjust Credit Limit:
* Ensures new limit covers existing drawn amount: `new_limit ≥ drawn`
* Requires controller signature

#### Accrue Interest:
* Allows interest calculation without other changes
* No additional signature requirements

### Helper Functions

#### `signedBy`
Verifies transaction signature by specific public key hash.

#### `elapsed`
Calculates time elapsed since last accrual.

#### `accrueInterest`
Calculates and applies interest based on elapsed time.

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
-- Validator written to: revolving-credit-line.plutus
-- 
-- --- Revolving Credit Line Validator Info ---
-- Validator Hash (Plutus): ValidatorHash...
-- Plutus Script Address:    Address...
-- Bech32 Script Address:    addr_test...
-- ---------------------------------
-- Revolving credit line validator generated successfully.
```

**Credit Line Operations:**

1. **Draw Funds:**
   - Borrow within credit limit
   - Authorization by borrower required

2. **Repay Balance:**
   - Reduce outstanding debt
   - Free up borrowing capacity

3. **Adjust Limits:**
   - Modify credit limits based on creditworthiness
   - Controller authorization required

4. **Accrue Interest:**
   - Apply time-based interest to outstanding balances
   - Can be triggered by anyone

---

## 7. 🧷 Testing Strategy

* Test borrowing within and beyond credit limits
* Verify interest accrual calculations over time
* Test limit adjustment authorization
* Validate repayment scenarios and partial repayments

---

## 8. ✅ Best Practices

* Implement robust interest rate models
* Set appropriate credit limits based on risk assessment
* Include proper authorization hierarchies
* Test with various time periods for interest accrual
* Implement comprehensive credit risk management

---

## 9. 📘 Glossary of Terms

| Term | Definition |
|------|------------|
| **Credit Line** | Pre-approved borrowing limit |
| **Drawn Amount** | Currently utilized portion of credit line |
| **Rate Model** | Parameters for interest calculation |
| **Accrual** | Interest application process |
| **Revolving Credit** | Reusable credit facility |
| **Controller** | Entity authorized to adjust limits |

---