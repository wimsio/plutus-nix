# 🔄 Detailed Tutorial: Understanding and Using Rebase Token Policy

This tutorial covers a sophisticated rebase token system with governance-controlled supply adjustments and minting/burning capabilities on the Cardano blockchain.

---

## 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [🗃️ Data Structures](#2-data-structures)
3. [🧠 Core Policy Logic](#3-core-policy-logic)
4. [⚙️ Policy Script Compilation](#4-policy-script-compilation)
5. [🔧 Helper Functions](#5-helper-functions)
6. [🧪 Practical Usage Example](#6-practical-usage-example)
7. [🧷 Testing Strategy](#7-testing-strategy)
8. [✅ Best Practices](#8-best-practices)
9. [📘 Glossary of Terms](#9-glossary-of-terms)

---

## 1. 📦 Imports Overview

### Plutus Core Modules

* **Plutus.V2.Ledger.Api:**
  Provides fundamental types: `PubKeyHash`, `CurrencySymbol`, `MintingPolicy`, `ScriptContext`, `TxInfo`.

* **Plutus.V2.Ledger.Contexts:**
  Contains utility functions for transaction context validation and minting operations.

* **PlutusTx.AssocMap:**
  Provides map data structures for value manipulation and analysis.

### Utility Modules

* **PlutusTx:**
  Enables script compilation and data serialization.

* **PlutusTx.Prelude:**
  Basic Plutus scripting functions and operators.

* **PlutusTx.Builtins:**
  Provides built-in functions for cryptographic operations and data conversion.

### Serialization & Cardano API

* **Codec.Serialise:**
  Handles script serialization for on-chain deployment.

* **Cardano.Api:**
  Provides network-specific address generation and Bech32 encoding.

---

## 2. 🗃️ Data Structures

### `RebaseDatum`

Defines the governance and parameter structure for the rebase token:

* `rdGovernors`: List of public key hashes authorized to perform governance actions
* `rdFactor`: Scaled rebase factor (e.g., 1.05 → 1050000 for precision)
* `rdMin`: Minimum allowed rebase factor (scaled)
* `rdMax`: Maximum allowed rebase factor (scaled)

### `RebaseAction`

Defines possible token operations:
* `Rebase Integer`: Adjust token supply with new scaling factor (governors only)
* `MintSome Integer`: Create new tokens (governors only)
* `BurnSome Integer`: Destroy existing tokens (anyone may burn)

---

## 3. 🧠 Core Policy Logic

### `mkPolicy`

**Purpose:** Controls token supply adjustments through governance mechanisms and user-initiated burns.

**Validation Conditions:**

#### Rebase (Supply Adjustment):
* Transaction must be signed by authorized governor
* New factor must be within allowed minimum and maximum bounds
* Enforces governance control over supply adjustments

#### MintSome (Token Creation):
* Transaction must be signed by authorized governor
* Mint amount must be positive
* Verifies minting operation matches specified amount
* Ensures controlled token issuance

#### BurnSome (Token Destruction):
* Burn amount must be positive
* Verifies burning operation matches specified amount
* Permissionless operation - anyone can burn tokens

### Helper Functions

#### `isGovernor`
Verifies transaction is signed by at least one authorized governor.

#### `findController`
Locates and extracts the controller datum from reference inputs.

#### `checkMint` & `checkBurn`
Validate that minting/burning operations match the specified amounts exactly.

---

## 4. ⚙️ Policy Script Compilation

### `mkPolicyUntyped`
Wraps the typed policy function for Plutus Core compatibility using `BuiltinData`.

### `policy`
Compiles the minting policy into a Plutus Core script ready for blockchain deployment.

### `currencySymbol`
Derives the currency symbol from the compiled policy script using SHA-256 hashing.

---

## 5. 🔧 Helper Functions

### `plutusValidatorHash`
Generates the script hash from the serialized minting policy.

### `toBech32PolicyAddress`
Generates human-readable Bech32 addresses for the policy script.

### `writePolicy`
Serializes and writes the minting policy to a `.plutus` file for deployment.

### Value Manipulation Functions

#### `flattenValue`
Converts Plutus `Value` type into a flat list of (CurrencySymbol, TokenName, Integer) tuples for easier analysis.

#### `extractDatum`
Retrieves datum information from transaction outputs with proper error handling.

---

## 6. 🧪 Practical Usage Example

```haskell
-- Generate and deploy the policy
main

-- Expected output:
-- Policy written to: rebase-policy.plutus
-- 
-- --- Rebase Token Policy ---
-- CurrencySymbol: CurrencySymbol...
-- Bech32 Script Address: addr_test...
-- ---------------------------------
-- Rebase token policy generated successfully.
```

**Token Operations:**

1. **Rebase (Governance Action):**
   - Adjust token supply scaling factor
   - Requires governor authorization
   - Factor must be within predefined bounds

2. **Mint Tokens (Governance Action):**
   - Create new tokens for circulation
   - Requires governor authorization
   - Positive amount verification

3. **Burn Tokens (User Action):**
   - Permanently remove tokens from circulation
   - Permissionless operation
   - Positive amount verification

**Governance Setup Example:**
```haskell
RebaseDatum
    { rdGovernors = ["gov_pkh_1", "gov_pkh_2", "gov_pkh_3"]
    , rdFactor = 1000000    -- 1.0 scaling
    , rdMin = 900000        -- 0.9 minimum
    , rdMax = 1100000       -- 1.1 maximum
    }
```

---

## 7. 🧷 Testing Strategy

* Test rebase operations with authorized and unauthorized governors
* Verify factor bounds enforcement
* Test minting operations with various amounts
* Validate burning operations from different users
* Test edge cases like zero amounts and boundary values
* Verify reference input requirements for controller datum

---

## 8. ✅ Best Practices

* Implement multi-signature governance for increased security
* Set conservative bounds for rebase factors to prevent extreme supply changes
* Use proper scaling factors for precision in mathematical operations
* Include comprehensive event logging for supply change tracking
* Implement gradual supply adjustment mechanisms to prevent market disruption
* Test governance transitions and key rotation scenarios

---

## 9. 📘 Glossary of Terms

| Term | Definition |
|------|------------|
| **Rebase** | Adjustment of token supply to maintain price stability or other metrics |
| **Governor** | Authorized entity with control over token supply parameters |
| **Scaling Factor** | Multiplier applied to token balances during rebase operations |
| **Minting Policy** | Script defining rules for token creation and destruction |
| **Currency Symbol** | Unique identifier derived from the minting policy script |
| **Reference Input** | UTxO referenced but not spent, used for datum access |

---