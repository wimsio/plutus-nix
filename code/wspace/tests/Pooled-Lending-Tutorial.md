# 🏊‍♂️ Detailed Tutorial: Understanding and Using Pooled Lending Smart Contract

This tutorial covers a decentralized pooled lending system with interest accrual and reserve management on the Cardano blockchain.

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
  Provides fundamental types: `POSIXTime`, `PubKeyHash`, `BuiltinByteString`, `ScriptContext`.

* **Plutus.V2.Ledger.Contexts:**
  Contains utility functions for transaction context validation.

* **Plutus.V1.Ledger.Interval:**
  Supplies interval functionality for time-based operations.

* **Plutus.V1.Ledger.Value:**
  Provides asset value operations for pool management.

### Utility Modules

* **PlutusTx:**
  Enables script compilation and data serialization.

* **PlutusTx.Prelude:**
  Basic Plutus scripting functions.

* **PlutusTx.Builtins:**
  Provides built-in functions for mathematical operations.

---

## 2. 🗃️ Data Structures

### `PoolDatum` (Pool State)

Defines the lending pool's on-chain state:

* `pdCash`: Total ADA available in pool
* `pdBorrows`: Total borrowed amount
* `pdReserveFactor`: Reserve factor (e.g., 10 = 10%)
* `pdRateModel`: Interest rate model parameters

### `PoolAction`

Defines possible pool operations:
* `Deposit Integer`: Add liquidity to pool
* `Withdraw Integer`: Remove liquidity from pool
* `Borrow Integer`: Borrow from pool
* `Repay Integer`: Repay borrowed amount
* `Accrue`: Update interest and reserves

---

## 3. 🧠 Core Validator Logic

### `mkValidator`

**Purpose:** Manages pooled lending operations with interest accrual and risk management.

**Validation Conditions:**

#### Deposit (Add Liquidity):
* Deposit amount must be greater than 0
* Increases available cash in pool
* Provides liquidity for borrowing

#### Withdraw (Remove Liquidity):
* Withdrawal amount ≤ available cash
* Maintains pool health and liquidity
* Allows liquidity providers to exit

#### Borrow (Take Loan):
* New borrows cannot exceed 80% utilization
* Borrow amount ≤ available cash
* Prevents over-borrowing from pool

#### Repay (Return Loan):
* Repay amount ≤ outstanding borrows
* Decreases total borrowed amount
* Includes accrued interest in repayment

#### Accrue (Update Interest):
* Can be called by anyone to update state
* Applies interest rate to outstanding borrows
* Moves portion to reserves based on reserve factor

### Helper Functions

#### `utilization`
Calculates pool utilization ratio as percentage.

#### `accrueInterest`
Calculates and applies interest with reserve allocation.

#### `after` & `signedBy`
Time and signature verification utilities.

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
-- Validator written to: pool-lending.plutus
-- 
-- --- Pooled Lending Validator Info ---
-- Validator Hash (Plutus): ValidatorHash...
-- Plutus Script Address:    Address...
-- Bech32 Script Address:    addr_test...
-- ---------------------------------
-- Pooled lending validator generated successfully.
```

**Lending Pool Operations:**

1. **Deposit Liquidity:**
   - Add funds to pool for interest earnings
   - Increase available lending capital
   - Positive amount required

2. **Withdraw Funds:**
   - Remove liquidity from pool
   - Within available cash limits
   - Maintain pool health

3. **Borrow Funds:**
   - Take loans against pool liquidity
   - Within utilization limits (max 80%)
   - Risk-managed borrowing

4. **Repay Loan:**
   - Return borrowed funds with interest
   - Within outstanding debt limits
   - Reduce personal debt position

5. **Accrue Interest:**
   - Update interest calculations
   - Allocate to reserves
   - Can be triggered by anyone

---

## 7. 🧷 Testing Strategy

* Test deposit and withdrawal scenarios
* Verify borrowing within utilization limits
* Test interest accrual calculations
* Validate reserve allocation mechanisms
* Test edge cases like zero balances and maximum utilization

---

## 8. ✅ Best Practices

* Implement robust interest rate models
* Set appropriate reserve factors for sustainability
* Maintain conservative utilization limits
* Include comprehensive risk management
* Test with various market conditions and stress scenarios

---

## 9. 📘 Glossary of Terms

| Term | Definition |
|------|------------|
| **Utilization** | Percentage of pool funds currently borrowed |
| **Reserve Factor** | Percentage of interest allocated to protocol reserves |
| **Rate Model** | Algorithm for calculating interest rates based on utilization |
| **Liquidity Pool** | Aggregated funds from multiple depositors |
| **Accrue** | Process of applying interest to outstanding loans |

---