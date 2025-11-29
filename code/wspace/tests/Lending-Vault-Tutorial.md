# 🏦 CDP Lending Vault Smart Contract

A Plutus smart contract for Collateralized Debt Position (CDP) lending with automated liquidations on the Cardano blockchain.

## 📖 Overview

This smart contract implements a sophisticated lending vault system where users can:
- **Lock collateral** to borrow assets against their value
- **Manage debt positions** with dynamic borrowing and repayment
- **Maintain safety ratios** to prevent under-collateralization
- **Enable liquidations** for positions below safety thresholds
- **Close positions** when debt is fully repaid

## 🏗️ Architecture

### Data Structures

#### Vault Datum
```haskell
data VaultDatum = VaultDatum
    { vdOwner    :: PubKeyHash    -- Vault owner's public key hash
    , vdCollValue :: Integer      -- Collateral value locked (in Lovelace)
    , vdDebt     :: Integer       -- Outstanding debt amount
    , vdMCR      :: Integer       -- Minimum Collateral Ratio (%)
    , vdLCR      :: Integer       -- Liquidation Collateral Ratio (%)
    }
```

#### Vault Actions
```haskell
data VaultAction
    = Open                     -- Create new vault position
    | Draw Integer            -- Borrow additional funds
    | Repay Integer           -- Repay outstanding debt
    | Close                   -- Close vault (zero debt)
    | Liquidate PubKeyHash    -- Liquidate under-collateralized vault
```

## 🔐 Validator Logic

### Core Validation Rules

#### ✅ Open Vault
- **Owner Authorization**: Transaction signed by vault owner
- **Initial Setup**: Establishes vault with collateral and risk parameters

#### ✅ Draw Funds (Borrow)
- **Owner Authorization**: Transaction signed by vault owner
- **MCR Compliance**: New debt position must maintain Minimum Collateral Ratio
- **Collateral Safety**: `collateralRatio(collateral, new_debt) ≥ MCR`

#### ✅ Repay Debt
- **Owner Authorization**: Transaction signed by vault owner
- **Ratio Maintenance**: New position maintains MCR OR debt reaches zero
- **Debt Reduction**: Prevents negative debt positions

#### ✅ Close Vault
- **Owner Authorization**: Transaction signed by vault owner
- **Zero Debt Requirement**: All debt must be repaid (`vdDebt == 0`)
- **Collateral Release**: Owner can reclaim collateral

#### ✅ Liquidate Vault
- **LCR Breach**: Vault must be below Liquidation Collateral Ratio
- **Liquidator Authorization**: Transaction signed by liquidator
- **Safety Trigger**: `collateralRatio(collateral, debt) < LCR`

### Helper Functions

#### `collateralRatio`
```haskell
collateralRatio coll debt = if debt == 0 then 100000 else (coll * 100) `divide` debt
```
Calculates collateral ratio as percentage (collateral/debt × 100).

#### `signedBy`
```haskell
signedBy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh
```
Verifies transaction signature by specific public key hash.

## 🚀 Usage

### Compilation & Deployment
```bash
# Compile the contract
cabal build

# Generate validator files
cabal run
```

### Expected Output
```
Validator written to: vault.plutus

--- CDP Lending Vault Info ---
Validator Hash: ValidatorHash...
Plutus Script Address: Address...
Bech32 Script Address: addr_test...
---------------------------------
```

### Transaction Flow

#### 1. Open Vault
```haskell
-- Action: Open
-- Signed by: Vault Owner
-- Requirements: Owner authorization only
-- Initializes vault with collateral and risk parameters
```

#### 2. Draw Funds
```haskell
-- Action: Draw amount
-- Signed by: Vault Owner
-- Requirements: 
--   - Owner signature
--   - collateralRatio(collateral, debt + amount) ≥ MCR
```

#### 3. Repay Debt
```haskell
-- Action: Repay amount  
-- Signed by: Vault Owner
-- Requirements:
--   - Owner signature
--   - collateralRatio(collateral, debt - amount) ≥ MCR OR new debt = 0
```

#### 4. Close Vault
```haskell
-- Action: Close
-- Signed by: Vault Owner
-- Requirements:
--   - Owner signature
--   - Zero outstanding debt (vdDebt == 0)
```

#### 5. Liquidate Vault
```haskell
-- Action: Liquidate liquidatorPkh
-- Signed by: Liquidator
-- Requirements:
--   - Liquidator signature
--   - collateralRatio(collateral, debt) < LCR
```

## 🛠️ Technical Details

### Dependencies
- **Plutus V2**: Core smart contract functionality
- **Cardano API**: Address generation and serialization
- **PlutusTx**: On-chain code compilation

### Key Features
- **Dual Ratio System**: Separate MCR (safety) and LCR (liquidation) thresholds
- **Flexible Borrowing**: Dynamic debt management within safe limits
- **Permissionless Liquidations**: Anyone can liquidate unsafe positions
- **Safe Closure**: Zero-debt requirement for position closure
- **Risk Management**: Automated protection against under-collateralization

## 📋 Prerequisites

- Cardano Node
- GHC 8.10+
- Cabal
- Plutus dependencies

## 🔧 Build Instructions

1. **Clone repository**
   ```bash
   git clone <repository-url>
   cd cdp-vault-contract
   ```

2. **Build project**
   ```bash
   cabal build
   ```

3. **Generate validator**
   ```bash
   cabal run
   ```

## 🧪 Testing

### Test Scenarios
- ✅ Successful vault creation
- ✅ Successful borrowing within MCR limits
- ✅ Successful debt repayment
- ✅ Successful vault closure with zero debt
- ✅ Successful liquidation of under-LCR vault
- ✅ Failed borrowing (exceeds MCR)
- ✅ Failed closure (outstanding debt)
- ✅ Failed liquidation (above LCR)

### Ratio Calculation Example
```haskell
-- Collateral: 1000 ADA, Debt: 500, MCR: 150%
-- Ratio = (1000 * 100) / 500 = 200% ✅ (above 150% MCR)

-- New Draw: 200 ADA → Total Debt: 700
-- New Ratio = (1000 * 100) / 700 = 142% ❌ (below 150% MCR)
```

## 🔒 Security Considerations

- **Ratio Enforcement**: Strict MCR compliance for all operations
- **Authorization Checks**: Proper signature verification for sensitive actions
- **Liquidation Safety**: Only allows liquidation when necessary for system health
- **Debt Management**: Prevents negative debt and ensures proper accounting
- **Collateral Protection**: Maintains sufficient collateral coverage

## 💰 Economic Model

### Key Parameters
- **Collateral Value (vdCollValue)**: Locked asset value in Lovelace
- **Debt (vdDebt)**: Borrowed amount outstanding
- **MCR (vdMCR)**: Minimum Collateral Ratio (%) for safe operations
- **LCR (vdLCR)**: Liquidation Collateral Ratio (%) triggering liquidations

### Risk Management
```
Safe Zone:    Ratio ≥ MCR    → Normal operations
Warning Zone: LCR ≤ Ratio < MCR → No new borrowing
Danger Zone:  Ratio < LCR    → Liquidatable
```

## 📊 Use Cases

### DeFi Protocols
- Over-collateralized lending platforms
- Stablecoin issuance systems
- Synthetic asset protocols

### Individual Users
- Leveraged trading positions
- Capital-efficient borrowing
- Asset utilization strategies

### Institutional
- Treasury management
- Collateral optimization
- Risk-managed lending operations

## ⚠️ Risk Parameters

### Typical Settings
- **MCR (Minimum Collateral Ratio)**: 150-200%
- **LCR (Liquidation Collateral Ratio)**: 110-130%
- **Buffer Zone**: 20-50% between MCR and LCR for safety

### Example Configuration
```haskell
VaultDatum
    { vdOwner = "owner_pkh"
    , vdCollValue = 1000000000  -- 1000 ADA
    , vdDebt = 500000000        -- 500 borrowed
    , vdMCR = 150               -- 150% MCR
    , vdLCR = 120               -- 120% LCR
    }
```

## 🔄 Workflow

1. **Initialization**: Create vault with collateral and risk parameters
2. **Borrowing**: Draw funds while maintaining MCR compliance
3. **Monitoring**: Track collateral ratio against market movements
4. **Management**: Repay debt or add collateral as needed
5. **Resolution**: Close position (zero debt) or face liquidation (below LCR)

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Submit a pull request

## 📄 License

This project is licensed under the [MIT License](LICENSE).

## 🆓 Glossary

| Term | Description |
|------|-------------|
| **CDP** | Collateralized Debt Position |
| **MCR** | Minimum Collateral Ratio (%) |
| **LCR** | Liquidation Collateral Ratio (%) |
| **Collateral Ratio** | (Collateral × 100) / Debt |
| **Liquidator** | Entity that closes under-collateralized positions |

---
