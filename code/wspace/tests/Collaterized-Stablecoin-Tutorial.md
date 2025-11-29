# 🏦 Detailed Tutorial: Understanding and Using Over-Collateralized Vault Smart Contract

This tutorial covers a MakerDAO-style vault system for creating decentralized, over-collateralized stablecoin positions on the Cardano blockchain.

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
  Provides fundamental types: `POSIXTime`, `PubKeyHash`, `Validator`, and transaction context.

* **Plutus.V2.Ledger.Contexts:**
  Contains utility functions for transaction context validation (`txSignedBy`).

* **Plutus.V1.Ledger.Value:**
  Provides asset value operations (`valueOf`, `adaSymbol`, `adaToken`, `geq`).

### Utility Modules

* **PlutusTx:**
  Enables script compilation and data serialization.

* **PlutusTx.Prelude:**
  Basic Plutus scripting functions and operators.

* **PlutusTx.Builtins:**
  Provides built-in functions for Plutus script operations.

---

## 2. 🗃️ Data Structures

### `VaultDatum`

Defines the vault's on-chain state:

* `vdOwner`: Vault owner's public key hash
* `vdColl`: Collateral locked (in lovelace or custom token)
* `vdDebt`: Stablecoin debt minted
* `vdMCR`: Minimum collateral ratio in %
* `vdRateIx`: Interest/stability fee index

### `VaultAction`

Defines possible vault operations:
* `Open`: Create new vault position
* `Draw Integer`: Borrow additional stablecoins
* `Repay Integer`: Repay outstanding debt
* `Liquidate`: Liquidate under-collateralized vault

---

## 3. 🧠 Core Validator Logic

### `mkVaultValidator`

**Purpose:** Ensures vault operations maintain system solvency and user safety.

**Validation Conditions:**

#### Open Vault:
* Ensures initial collateral ratio meets minimum requirements

#### Draw Stablecoins:
* Checks that increased debt maintains collateral ratio
* `collateral >= (new_debt * MCR) / 100`

#### Repay Debt:
* Prevents negative debt balances
* `new_debt >= 0`

#### Liquidate Vault:
* Only allows liquidation when vault is under-collateralized
* `collateral_ratio < minimum_required_ratio`

### Helper Functions

#### `collateralRatio`
Calculates if vault meets minimum collateral requirements.

#### `scriptInputContainsCollateral`
Verifies sufficient collateral exists in the script UTxO.

---

## 4. ⚙️ Validator Script Compilation

### `mkVaultValidatorUntyped`
Wraps the core validator function for Plutus Core compatibility.

### `validator`
Compiles the validator into a Plutus Core script ready for blockchain deployment.

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
-- Validator written to: vault.plutus
-- 
-- --- Vault Validator Info ---
-- Validator Hash (Plutus): ValidatorHash...
-- Plutus Script Address:    Address...
-- Bech32 Script Address:    addr_test...
-- ---------------------------------
-- Over-collateralized stablecoin vault generated successfully.
```

**Vault Operations:**

1. **Open Vault:**
   - Lock collateral to create new position
   - Ensure initial ratio meets MCR

2. **Draw Funds:**
   - Borrow stablecoins against collateral
   - Maintain collateral ratio above MCR

3. **Repay Debt:**
   - Return stablecoins to reduce debt
   - Improve collateral ratio

4. **Liquidate:**
   - Close under-collateralized positions
   - Protect system from bad debt

---

## 7. 🧷 Testing Strategy

* Test collateral ratio calculations with various inputs
* Verify liquidation triggers at correct thresholds
* Test edge cases like zero debt and maximum borrowing
* Validate signature requirements for all operations

---

## 8. ✅ Best Practices

* Set conservative minimum collateral ratios for safety
* Implement proper interest rate mechanisms
* Include comprehensive liquidation procedures
* Test with volatile market scenarios
* Provide clear error messages for failed transactions

---

## 9. 📘 Glossary of Terms

| Term | Definition |
|------|------------|
| **Collateral** | Assets locked in vault to secure debt |
| **Debt** | Stablecoins minted against collateral |
| **MCR** | Minimum Collateral Ratio (%) |
| **Liquidation** | Forced closure of under-collateralized vault |
| **Collateral Ratio** | (Collateral × 100) / Debt |
| **Stablecoin** | Price-stable cryptocurrency pegged to assets |

---