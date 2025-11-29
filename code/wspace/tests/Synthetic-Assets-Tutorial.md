# 📈 Detailed Tutorial: Understanding and Using Synthetic Asset Validator

This tutorial covers a sophisticated synthetic asset system that enables users to mint synthetic assets against collateral with oracle-based price feeds and liquidation mechanisms on the Cardano blockchain.

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
  Provides fundamental types: `POSIXTime`, `PubKeyHash`, `Validator`, `ScriptContext`, `TxInfo`.

* **Plutus.V2.Ledger.Contexts:**
  Contains utility functions for transaction context validation and reference inputs.

* **Plutus.V1.Ledger.Interval:**
  Supplies interval functionality for time-based operations and freshness checks.

* **Plutus.V1.Ledger.Value:**
  Provides asset value operations for collateral and debt tracking.

### Utility Modules

* **PlutusTx:**
  Enables script compilation and data serialization.

* **PlutusTx.Prelude:**
  Basic Plutus scripting functions and mathematical operations.

* **PlutusTx.Builtins:**
  Provides built-in functions for data conversion and operations.

### Serialization & Cardano API

* **Codec.Serialise:**
  Handles script serialization for on-chain deployment.

* **Cardano.Api:**
  Provides network-specific address generation and Bech32 encoding.

---

## 2. 🗃️ Data Structures

### `SynthVault`

Defines the synthetic asset vault's on-chain state:

* `svOwner`: Vault owner's public key hash
* `svCollateral`: Amount of ADA locked as collateral (in Lovelace)
* `svDebt`: Amount of synthetic assets minted against collateral
* `svMinRatio`: Minimum collateralization ratio (e.g., 150% = 150)

### `PriceDatum`

Defines the oracle price feed data structure:

* `pdPrice`: Current price in Lovelace per synthetic asset unit
* `pdOracle`: Oracle provider's public key hash for signature verification
* `pdUpdated`: Timestamp of last price update

### `SynthAction`

Defines possible synthetic asset operations:
* `MintSynth Integer`: Mint new synthetic assets against collateral
* `BurnSynth Integer`: Burn synthetic assets to reduce debt
* `UpdatePrice`: Update the price oracle data
* `Liquidate`: Liquidate under-collateralized vaults

---

## 3. 🧠 Core Validator Logic

### `mkValidator`

**Purpose:** Manages synthetic asset minting, burning, and liquidation with real-time price feeds and collateral ratio enforcement.

**Validation Conditions:**

#### MintSynth (Create Synthetic Assets):
* Transaction must be signed by vault owner
* Oracle price must be fresh (within 30,000 ms of current time)
* New collateral ratio must meet minimum requirements after minting
* Output vault must reflect updated debt amount

#### BurnSynth (Reduce Debt):
* Transaction must be signed by vault owner
* Output vault must reflect reduced debt amount
* No minimum ratio check (burning always improves ratio)

#### UpdatePrice (Oracle Operation):
* Transaction must be signed by authorized oracle provider
* Ensures only trusted sources can update price data

#### Liquidate (Risk Management):
* Vault must be under-collateralized based on current price
* No signature requirements (permissionless liquidation)
* Protects system from bad debt accumulation

### Helper Functions

#### `getRefOracle`
Retrieves and validates oracle data from reference inputs with proper error handling.

#### `ratioOK`
Calculates whether vault maintains sufficient collateralization ratio.

#### `lowerBoundTime`
Extracts the lower bound timestamp from transaction validity range.

#### `getOutputDatum`
Reads and validates the updated vault state from transaction outputs.

---

## 4. ⚙️ Validator Script Compilation

### `mkValidatorUntyped`
Wraps the typed validator function for Plutus Core compatibility using `BuiltinData`.

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
-- Validator written to: synth-validator.plutus
-- 
-- --- Synthetic Asset Validator Info ---
-- Validator Hash: ValidatorHash...
-- Plutus Address: Address...
-- Bech32 Address: addr_test...
-- ---------------------------------------
-- Synthetic-asset validator generated successfully.
```

**Synthetic Asset Operations:**

1. **Mint Synthetic Assets:**
   ```haskell
   -- Action: MintSynth amount
   -- Requirements:
   --   - Vault owner signature
   --   - Fresh oracle price (within 30 seconds)
   --   - Maintain minimum collateral ratio
   --   - Update vault debt in output
   ```

2. **Burn Synthetic Assets:**
   ```haskell
   -- Action: BurnSynth amount
   -- Requirements:
   --   - Vault owner signature
   --   - Reduce debt amount in output vault
   --   - No ratio check (burning improves ratio)
   ```

3. **Update Price Oracle:**
   ```haskell
   -- Action: UpdatePrice
   -- Requirements:
   --   - Oracle provider signature
   --   - Reference input with new price data
   ```

4. **Liquidate Vault:**
   ```haskell
   -- Action: Liquidate
   -- Requirements:
   --   - Vault below minimum collateral ratio
   --   - Permissionless operation
   --   - Current oracle price
   ```

**Collateral Ratio Calculation:**
```haskell
-- Example: 1000 ADA collateral, 500 synthetic assets, Price: 2 ADA per synth
-- Collateral Value = 1000 ADA
-- Debt Value = 500 × 2 = 1000 ADA
-- Ratio = (1000 × 100) / 1000 = 100% (below 150% minimum → liquidatable)
```

---

## 7. 🧷 Testing Strategy

* Test minting operations with various collateral ratios
* Verify oracle freshness requirements (30-second window)
* Test liquidation triggers at correct ratio thresholds
* Validate signature requirements for all operations
* Test edge cases like zero debt and maximum minting
* Verify reference input handling for oracle data

---

## 8. ✅ Best Practices

* Use multiple trusted oracles for price feeds to prevent manipulation
* Implement circuit breakers for extreme market volatility
* Set conservative minimum collateral ratios (150-200%)
* Include time delays for critical operations like liquidation
* Implement proper oracle key rotation and multi-signature schemes
* Test with realistic price movement scenarios and flash crash conditions

---

## 9. 📘 Glossary of Terms

| Term | Definition |
|------|------------|
| **Synthetic Asset** | Token that tracks the value of an underlying asset without direct ownership |
| **Collateral Ratio** | (Collateral Value × 100) / Debt Value |
| **Oracle** | Trusted data source providing external price information |
| **Liquidation** | Forced closure of under-collateralized positions to protect system solvency |
| **Reference Input** | UTxO referenced but not spent, used for oracle data access |
| **Minimum Ratio** | Lowest allowed collateralization percentage before liquidation |

---

