# Changelog

## [1.0.1] - 2025-10-20

### 🚀 Overview
This release refines the Plutus vesting validator logic, improves cross-version compatibility, and enhances off-chain utility integration for smoother testing and serialization.  

**Contributors:** Bernard Sibanda  

---

### ✨ Added
- Integrated **advanced security checks** in `mkVestingValidator`:
  - Beneficiary signature verification  
  - Deadline and validity window limits  
  - Code integrity matching to prevent double spend  
  - Output address consistency and datum type safety  
- Added `Utilities.hs` module for:
  - `wrapValidator`, `writeValidatorToFile`, and `validatorAddressBech32`
  - `posixTimeFromIso8601` for ISO 8601 → POSIX conversion  
  - `printDataToJSON` for readable datum output  
- Introduced support for **Bech32 address export** and **local validator file generation**

---

### 🧩 Fixed
- Compatibility with `plutus-ledger-api-1.0.0.1` (removed V3+ API dependencies)
- Missing imports for:
  - `Prelude (Show, Integer, IO, String, fromJust)`
  - `Data.Maybe (fromJust)`
  - `PlutusTx.Prelude (mempty, (+))`
  - `Cardano.Api (NetworkId(..), NetworkMagic(..))`
- Corrected `traceIfFalse` string type errors by enabling `OverloadedStrings`
- Fixed `fromBuiltinData`, `txOutDatumHash`, and `findDatum` scope errors

---

### 🔧 Changed
- Simplified validity window checks (`interval` and `contains`)
- Reverted to V2 context-style validator for stability
- Consolidated helper utilities under one consistent import path
- Improved documentation and indentation throughout validator code

---

### 👥 Contributors
- **Bernard Sibanda** — core validator refactor, utility integration, and release preparation
