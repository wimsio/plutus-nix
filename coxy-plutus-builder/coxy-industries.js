// coxy-industries.js
export const industries = [
  {
    id: "Coxy",
    label: "Coxy / Marketplace",
    datumType: "CoxyDatum",
    redeemerType: "CoxyRedeemer",

    // Datum fields relevant for Coxy
    datumFields: [
      { id: "seller",   label: "seller :: PubKeyHash",        type: "PubKeyHash" },
      { id: "buyer",    label: "buyer :: PubKeyHash",         type: "PubKeyHash" },
      { id: "amount",   label: "amount :: Integer",           type: "Integer" },
      { id: "deadline", label: "deadline :: POSIXTime",       type: "POSIXTime" },
      { id: "currency", label: "currency :: CurrencySymbol",  type: "CurrencySymbol" },
      { id: "token",    label: "token :: TokenName",          type: "TokenName" }
    ],

    actions: [
      { id: "PaySeller",   label: "Pay Seller" },
      { id: "RefundBuyer", label: "Refund Buyer" }
    ],

    constraints: [
      { id: "signedByBuyer",   label: "Signed by buyer" },
      { id: "signedBySeller",  label: "Signed by seller" },
      { id: "beforeDeadline",  label: "Before deadline" },
      { id: "afterDeadline",   label: "After deadline" },
      { id: "sellerPaid",      label: "Seller paid full amount" },
      { id: "buyerRefunded",   label: "Buyer fully refunded" },
      { id: "scriptHasNFT",    label: "Script UTxO contains NFT" }
    ],

    defaultActionConstraints: {
      PaySeller:   ["scriptHasNFT", "signedByBuyer", "sellerPaid"],
      RefundBuyer: ["scriptHasNFT", "signedBySeller", "buyerRefunded", "afterDeadline"]
    }
  },

  // -------------------------------------------------------------
  // Membership SBT / DAO industry
  // -------------------------------------------------------------
  {
    id: "membership",
    label: "Membership SBT / DAO",
    datumType: "MemberDatum",
    redeemerType: "MemberAction",

    // MemberSBT{org, member, joined, expiry, level}
    datumFields: [
      { id: "org",     label: "org :: CurrencySymbol", type: "CurrencySymbol" },
      { id: "member",  label: "member :: PubKeyHash",  type: "PubKeyHash" },
      { id: "joined",  label: "joined :: POSIXTime",   type: "POSIXTime" },
      { id: "expiry",  label: "expiry :: POSIXTime",   type: "POSIXTime" },
      { id: "level",   label: "level :: Integer",      type: "Integer" }
    ],

    // Join/renew/revoke membership
    actions: [
      { id: "Join",   label: "Join" },
      { id: "Renew",  label: "Renew" },
      { id: "Revoke", label: "Revoke" }
    ],

    // Constraints must match MembershipIR parseConstraintId
    constraints: [
      { id: "signedByMember",   label: "Signed by member" },
      { id: "signedByOrgAdmin", label: "Signed by org admin" },
      { id: "notExpired",       label: "Membership not expired" },
      { id: "afterExpiry",      label: "After expiry" },
      { id: "validLevelRange",  label: "Level in allowed range" }
    ],

    // Default recommendations for students
    defaultActionConstraints: {
      Join:   ["signedByMember", "validLevelRange"],
      Renew:  ["signedByMember", "notExpired", "validLevelRange"],
      Revoke: ["signedByOrgAdmin", "afterExpiry"]
    }
  },

  // -------------------------------------------------------------
  // Constant-product AMM (x·y = k) pool
  // -------------------------------------------------------------
  {
    id: "constantProductAmm",
    label: "Constant-product AMM (x·y = k)",
    datumType: "CpPoolDatum",
    redeemerType: "CpPoolRedeemer",

    // PoolDatum { resA, resB, feeBps, lpPolicyId, poolNFT }
    datumFields: [
      { id: "resA",       label: "resA :: Integer",               type: "Integer" },
      { id: "resB",       label: "resB :: Integer",               type: "Integer" },
      { id: "feeBps",     label: "feeBps :: Integer",             type: "Integer" },
      { id: "lpPolicyId", label: "lpPolicyId :: CurrencySymbol",  type: "CurrencySymbol" },
      { id: "poolNFT",    label: "poolNFT :: CurrencySymbol",     type: "CurrencySymbol" }
    ],

    // Swap in A or B, add/remove liquidity
    actions: [
      { id: "SwapInA",    label: "Swap (token A → B)" },
      { id: "SwapInB",    label: "Swap (token B → A)" },
      { id: "AddLiq",     label: "Add Liquidity" },
      { id: "RemoveLiq",  label: "Remove Liquidity" }
    ],

    // Constraints / invariants for the pool script + LP policy
    constraints: [
      {
        id: "hasPoolNFT",
        label: "Tx spends & re-creates unique pool UTxO carrying poolNFT"
      },
      {
        id: "conservesResWithFee",
        label: "Reserves updated with conservation of value plus fee"
      },
      {
        id: "kNonDecreasing",
        label: "Post-fee product x·y is non-decreasing"
      },
      {
        id: "lpMintBurnWithPool",
        label: "LP tokens mint/burn only when spending the pool UTxO"
      },
      {
        id: "lpProportionalShare",
        label: "LP mint/burn proportional to pool share"
      }
    ],

    // Default teaching presets
    defaultActionConstraints: {
      SwapInA:   ["hasPoolNFT", "conservesResWithFee", "kNonDecreasing"],
      SwapInB:   ["hasPoolNFT", "conservesResWithFee", "kNonDecreasing"],
      AddLiq:    ["hasPoolNFT", "lpMintBurnWithPool", "lpProportionalShare"],
      RemoveLiq: ["hasPoolNFT", "lpMintBurnWithPool", "lpProportionalShare"]
    }
  },

  // -------------------------------------------------------------
  // Concentrated-liquidity AMM (Uniswap v3-style)
  // -------------------------------------------------------------
  {
    id: "concentratedLiquidityAmm",
    label: "Concentrated-liquidity AMM (Uniswap v3-style)",
    datumType: "ClPoolDatum",
    redeemerType: "ClPoolRedeemer",

    // PoolDatum { currentTick, sqrtPriceX96, feeBps, poolNFT }
    // (Tick/Position datums live in separate scripts / policies)
    datumFields: [
      { id: "currentTick",  label: "currentTick :: Integer",    type: "Integer" },
      { id: "sqrtPriceX96", label: "sqrtPriceX96 :: Integer",   type: "Integer" },
      { id: "feeBps",       label: "feeBps :: Integer",         type: "Integer" },
      { id: "poolNFT",      label: "poolNFT :: CurrencySymbol", type: "CurrencySymbol" }
    ],

    // High-level pool actions (ticks/positions updated along the way)
    actions: [
      { id: "Swap",        label: "Swap" },
      { id: "MintPos",     label: "Mint Position" },
      { id: "BurnPos",     label: "Burn Position" },
      { id: "CollectFees", label: "Collect Fees" }
    ],

    constraints: [
      {
        id: "uniquePoolViaNFT",
        label: "Unique pool UTxO enforced via poolNFT"
      },
      {
        id: "validTickMath",
        label: "Tick/sqrtPrice updates follow deterministic math"
      },
      {
        id: "updatesLiquidityNet",
        label: "Liquidity net per tick updated consistently with swaps/positions"
      },
      {
        id: "positionNotOverCollected",
        label: "Positions cannot collect more fees than owed"
      }
    ],

    defaultActionConstraints: {
      Swap:        ["uniquePoolViaNFT", "validTickMath", "updatesLiquidityNet"],
      MintPos:     ["uniquePoolViaNFT", "validTickMath", "updatesLiquidityNet"],
      BurnPos:     ["uniquePoolViaNFT", "validTickMath", "updatesLiquidityNet"],
      CollectFees: ["uniquePoolViaNFT", "positionNotOverCollected"]
    }
  },

  // -------------------------------------------------------------
  // Stable-swap AMM (Curve-like)
  // -------------------------------------------------------------
  {
    id: "stableSwapAmm",
    label: "Stable-swap AMM (Curve-like)",
    datumType: "StablePoolDatum",
    redeemerType: "StablePoolRedeemer",

    // PoolDatum { balances[2..N], ampParam, feeBps, adminRef }
    // NOTE: if your UI only supports primitive types, you may need to special-case these.
    datumFields: [
      // Use a generic Integer for “index into balances” UI; underlying Haskell will use [Integer].
      { id: "ampParam", label: "ampParam :: Integer",   type: "Integer" },
      { id: "feeBps",   label: "feeBps :: Integer",     type: "Integer" },
      // Represent adminRef as CurrencySymbol or TokenName in UI; real script will use TxOutRef.
      { id: "adminRef", label: "adminRef :: CurrencySymbol", type: "CurrencySymbol" }
    ],

    actions: [
      { id: "Swap",               label: "Swap (i → j)" },
      { id: "AddLiq",             label: "Add Liquidity" },
      { id: "RemoveOne",          label: "Remove One Coin" },
      { id: "RemoveProportional", label: "Remove Proportional" }
    ],

    constraints: [
      {
        id: "uniquePoolStable",
        label: "Unique pool UTxO for this stable-swap pool"
      },
      {
        id: "hasAdminRef",
        label: "Tx uses admin/param reference input for ampParam"
      },
      {
        id: "ampAndFeeBounded",
        label: "Amplification and fee parameters are within allowed bounds"
      },
      {
        id: "stableSwapWithinTolerance",
        label: "Output amounts satisfy StableSwap invariant within tolerance"
      }
    ],

    defaultActionConstraints: {
      Swap:               ["uniquePoolStable", "hasAdminRef", "ampAndFeeBounded", "stableSwapWithinTolerance"],
      AddLiq:             ["uniquePoolStable", "hasAdminRef", "ampAndFeeBounded"],
      RemoveOne:          ["uniquePoolStable", "hasAdminRef", "ampAndFeeBounded", "stableSwapWithinTolerance"],
      RemoveProportional: ["uniquePoolStable", "hasAdminRef", "ampAndFeeBounded", "stableSwapWithinTolerance"]
    }
  },  // -------------------------------------------------------------
  // Batch-auction / order-book DEX
  // -------------------------------------------------------------
  {
    id: "batchAuctionDex",
    label: "Batch-auction / Order-book DEX",
    datumType: "OrderDatum",
    redeemerType: "OrderRedeemer",

    // OrderDatum { base, quote, side, priceNum, priceDen, qty, owner }
    datumFields: [
      { id: "base",     label: "base :: CurrencySymbol",   type: "CurrencySymbol" },
      { id: "quote",    label: "quote :: CurrencySymbol",  type: "CurrencySymbol" },
      { id: "side",     label: "side :: Integer  -- 0=Buy,1=Sell", type: "Integer" },
      { id: "priceNum", label: "priceNum :: Integer",      type: "Integer" },
      { id: "priceDen", label: "priceDen :: Integer",      type: "Integer" },
      { id: "qty",      label: "qty :: Integer",           type: "Integer" },
      { id: "owner",    label: "owner :: PubKeyHash",      type: "PubKeyHash" }
    ],

    // Cancel order, or include it in a batch Match
    actions: [
      { id: "Cancel", label: "Cancel Order" },
      { id: "Match",  label: "Batch Match" }
    ],

    constraints: [
      {
        id: "signedByOwner",
        label: "Cancel tx signed by order owner"
      },
      {
        id: "withinLimitPrice",
        label: "Filled price respects order limit (≤ for buys, ≥ for sells)"
      },
      {
        id: "feesExact",
        label: "Trading and protocol fees are exactly accounted for"
      },
      {
        id: "leftoversReturned",
        label: "Unfilled quantity is returned to order owner"
      },
      {
        id: "deterministicProRata",
        label: "Ties filled pro-rata in a deterministic way"
      },
      {
        id: "matcherNoSpread",
        label: "Matcher cannot extract price spread beyond fees"
      }
    ],

    defaultActionConstraints: {
      Cancel: ["signedByOwner"],
      Match: [
        "withinLimitPrice",
        "feesExact",
        "leftoversReturned",
        "deterministicProRata",
        "matcherNoSpread"
      ]
    }
  },

  // -------------------------------------------------------------
  // TWAMM (Time-Weighted AMM long order)
  // -------------------------------------------------------------
  {
    id: "twamm",
    label: "TWAMM (Time-Weighted AMM)",
    datumType: "TwammOrderDatum",
    redeemerType: "TwammAction",

    // LongOrder NFT + Datum { owner, direction, total, start, end, executed }
    datumFields: [
      { id: "owner",     label: "owner :: PubKeyHash",       type: "PubKeyHash" },
      { id: "direction", label: "direction :: Integer  -- 0=A→B,1=B→A", type: "Integer" },
      { id: "total",     label: "total :: Integer",          type: "Integer" },
      { id: "start",     label: "start :: POSIXTime",        type: "POSIXTime" },
      { id: "end",       label: "end :: POSIXTime",          type: "POSIXTime" },
      { id: "executed",  label: "executed :: Integer",       type: "Integer" }
    ],

    actions: [
      { id: "ExecuteSlice", label: "Execute Time Slice" },
      { id: "Cancel",       label: "Cancel Remaining" }
    ],

    constraints: [
      {
        id: "signedByOwner",
        label: "Cancel tx signed by long-order owner"
      },
      {
        id: "validExecutionRate",
        label: "Execution rate = total / (end - start)"
      },
      {
        id: "notOverExecuted",
        label: "Cumulative executed amount ≤ total"
      },
      {
        id: "withinScheduleWindow",
        label: "Slice execution only between start and end"
      },
      {
        id: "poolMathConsistent",
        label: "Pool reserves & virtual reserves follow TWAMM math"
      }
    ],

    defaultActionConstraints: {
      ExecuteSlice: [
        "validExecutionRate",
        "notOverExecuted",
        "withinScheduleWindow",
        "poolMathConsistent"
      ],
      Cancel: [
        "signedByOwner",
        "notOverExecuted"
      ]
    }
  },

  // -------------------------------------------------------------
  // RFQ / OTC swap escrow (maker–taker)
  // -------------------------------------------------------------
  {
    id: "rfqEscrow",
    label: "RFQ / OTC Escrow (Maker–Taker)",
    datumType: "EscrowDatum",
    redeemerType: "EscrowAction",

    // EscrowDatum { maker, taker, quoteHash, deposit }
    datumFields: [
      { id: "maker",    label: "maker :: PubKeyHash",      type: "PubKeyHash" },
      { id: "taker",    label: "taker :: PubKeyHash",      type: "PubKeyHash" },
      { id: "quoteHash",label: "quoteHash :: TokenName",   type: "TokenName" },
      { id: "deposit",  label: "deposit :: Integer",       type: "Integer" }
    ],

    actions: [
      { id: "Fill",   label: "Fill Quote" },
      { id: "Cancel", label: "Cancel Escrow" }
    ],

    constraints: [
      {
        id: "quoteNotExpired",
        label: "Quote expiry time not passed"
      },
      {
        id: "correctAssetsAndAmounts",
        label: "Escrow and taker payment match quote assets and amounts"
      },
      {
        id: "makerSigOverQuote",
        label: "Maker signature is valid over the quoted terms"
      },
      {
        id: "nonceNotReused",
        label: "Quote nonce has not been used before (one-time)"
      },
      {
        id: "signedByMaker",
        label: "Cancel tx signed by maker"
      }
    ],

    defaultActionConstraints: {
      Fill: [
        "quoteNotExpired",
        "correctAssetsAndAmounts",
        "makerSigOverQuote",
        "nonceNotReused"
      ],
      Cancel: [
        "signedByMaker",
        "nonceNotReused"
      ]
    }
  },

  // -------------------------------------------------------------
  // Multi-hop router (cross-pool in one tx)
  // -------------------------------------------------------------
  {
    id: "multiHopRouter",
    label: "Multi-hop Router (Cross-pool)",
    datumType: "RouterDatum",
    redeemerType: "RouterAction",

    // RouterDatum could be empty or hold config off-chain; keep no fields for now
    datumFields: [],

    actions: [
      { id: "Route", label: "Execute Route" }
    ],

    constraints: [
      {
        id: "pathSequential",
        label: "Route hops form a valid sequential path of assets and pools"
      },
      {
        id: "minOutRespected",
        label: "Final output amount ≥ minOut specified in redeemer"
      },
      {
        id: "noUnintendedValueExtraction",
        label: "No intermediate hop extracts unintended value"
      }
    ],

    defaultActionConstraints: {
      Route: [
        "pathSequential",
        "minOutRespected",
        "noUnintendedValueExtraction"
      ]
    }
  },

  // -------------------------------------------------------------
  // LP token mint/burn policy (proportional shares)
  // -------------------------------------------------------------
  {
    id: "lpTokenPolicy",
    label: "LP Token Policy (Proportional Shares)",
    datumType: "LpPolicyDatum",
    redeemerType: "LpPolicyAction",

    // Minting policies normally don't use datum; keep none for UI
    datumFields: [],

    actions: [
      { id: "MintLP", label: "Mint LP on Add Liquidity" },
      { id: "BurnLP", label: "Burn LP on Remove Liquidity" }
    ],

    constraints: [
      {
        id: "requiresPoolUTxO",
        label: "Mint/burn requires spending the pool UTxO with poolNFT"
      },
      {
        id: "lpMatchesFormula",
        label: "ΔLP follows proportional share formula from reserve deltas"
      },
      {
        id: "noMintWithoutReserves",
        label: "Cannot mint LP when reserves are zero or uninitialized"
      },
      {
        id: "totalLpPositive",
        label: "Total LP supply remains > 0 after init"
      }
    ],

    defaultActionConstraints: {
      MintLP: [
        "requiresPoolUTxO",
        "lpMatchesFormula",
        "noMintWithoutReserves",
        "totalLpPositive"
      ],
      BurnLP: [
        "requiresPoolUTxO",
        "lpMatchesFormula",
        "totalLpPositive"
      ]
    }
  },

  // -------------------------------------------------------------
  // Liquidity mining gauge (epoch reward distributor)
  // -------------------------------------------------------------
  {
    id: "liquidityGauge",
    label: "Liquidity Mining Gauge (Rewards)",
    datumType: "GaugeDatum",
    redeemerType: "GaugeAction",

    // GaugeDatum { epoch, totalWeight, rewardRate, adminRef }
    datumFields: [
      { id: "epoch",       label: "epoch :: Integer",          type: "Integer" },
      { id: "totalWeight", label: "totalWeight :: Integer",    type: "Integer" },
      { id: "rewardRate",  label: "rewardRate :: Integer",     type: "Integer" },
      { id: "adminRef",    label: "adminRef :: CurrencySymbol",type: "CurrencySymbol" }
    ],

    actions: [
      { id: "Stake",       label: "Stake LP" },
      { id: "Unstake",     label: "Unstake LP" },
      { id: "Claim",       label: "Claim Rewards" },
      { id: "AdminUpdate", label: "Admin Update Params" }
    ],

    constraints: [
      {
        id: "correctRewardAccrual",
        label: "Reward growth per second/epoch computed correctly"
      },
      {
        id: "noOverDistribution",
        label: "Total rewards distributed do not exceed emissions"
      },
      {
        id: "weightAccountingCorrect",
        label: "Stakes/unstakes update totalWeight consistently"
      },
      {
        id: "adminUpdateWithDelay",
        label: "Admin updates require delayed governance reference input"
      },
      {
        id: "signedByAdmin",
        label: "Admin updates signed by controller/admin"
      }
    ],

    defaultActionConstraints: {
      Stake: [
        "weightAccountingCorrect",
        "correctRewardAccrual",
        "noOverDistribution"
      ],
      Unstake: [
        "weightAccountingCorrect",
        "correctRewardAccrual",
        "noOverDistribution"
      ],
      Claim: [
        "correctRewardAccrual",
        "noOverDistribution"
      ],
      AdminUpdate: [
        "signedByAdmin",
        "adminUpdateWithDelay"
      ]
    }
  },

  // -------------------------------------------------------------
  // Protocol fee router / treasury splitter
  // -------------------------------------------------------------
  {
    id: "protocolFeeRouter",
    label: "Protocol Fee Router / Treasury Splitter",
    datumType: "FeeRouterDatum",
    redeemerType: "FeeRouterAction",

    // RouterDatum { splits[{dest,bps}], controller, version, routerNFT }
    // Splits array is handled off-chain; store controller/version/routerNFT primitives.
    datumFields: [
      { id: "controller", label: "controller :: PubKeyHash",    type: "PubKeyHash" },
      { id: "version",    label: "version :: Integer",          type: "Integer" },
      { id: "routerNFT",  label: "routerNFT :: CurrencySymbol", type: "CurrencySymbol" }
    ],

    actions: [
      { id: "Route",        label: "Route Fees to Destinations" },
      { id: "UpdateSplits", label: "Update Split Config" }
    ],

    constraints: [
      {
        id: "bpsSumTo10000",
        label: "Configured splits basis points sum to exactly 10,000"
      },
      {
        id: "exactConservation",
        label: "Total inputs equal sum of outputs (no value lost or created)"
      },
      {
        id: "uniqueRouterNFT",
        label: "Single authoritative router instance via routerNFT"
      },
      {
        id: "governanceRefRequired",
        label: "Split updates require governance reference input"
      },
      {
        id: "updateTimelocked",
        label: "Split updates subject to governance timelock"
      }
    ],

    defaultActionConstraints: {
      Route: [
        "bpsSumTo10000",
        "exactConservation",
        "uniqueRouterNFT"
      ],
      UpdateSplits: [
        "bpsSumTo10000",
        "uniqueRouterNFT",
        "governanceRefRequired",
        "updateTimelocked"
      ]
    }
  }
  ,
    // -------------------------------------------------------------
  // 11) On-chain pathing guard (slippage + min-out checks)
  // -------------------------------------------------------------
  {
    id: "pathingGuard",
    label: "On-chain Pathing Guard",
    datumType: "RouteDatum",
    redeemerType: "RouteAction",

    // RouteDatum { hops:[PoolId], inAsset, outAsset, minOut, deadline }
    // Hops are validated via pathProof in redeemer; we store key params.
    datumFields: [
      { id: "inAsset",   label: "inAsset :: CurrencySymbol",   type: "CurrencySymbol" },
      { id: "outAsset",  label: "outAsset :: CurrencySymbol",  type: "CurrencySymbol" },
      { id: "minOut",    label: "minOut :: Integer",           type: "Integer" },
      { id: "deadline",  label: "deadline :: POSIXTime",       type: "POSIXTime" }
    ],

    actions: [
      { id: "Execute", label: "Execute Route" }
    ],

    constraints: [
      {
        id: "allHopsPresent",
        label: "All route hops proved present and consistent with pathProof"
      },
      {
        id: "outputsMeetMinOut",
        label: "Final output amount ≥ minOut"
      },
      {
        id: "beforeDeadline",
        label: "Current time ≤ deadline"
      },
      {
        id: "noExtraValueSiphoned",
        label: "No extra value siphoned by intermediate addresses"
      }
    ],

    defaultActionConstraints: {
      Execute: [
        "allHopsPresent",
        "outputsMeetMinOut",
        "beforeDeadline",
        "noExtraValueSiphoned"
      ]
    }
  },

  // -------------------------------------------------------------
  // 12) On-chain limit order UTxO (price+qty guard)
  // -------------------------------------------------------------
  {
    id: "onchainLimitOrder",
    label: "On-chain Limit Order",
    datumType: "LimitOrderDatum",
    redeemerType: "LimitOrderAction",

    // OrderDatum { owner, base, quote, side, limitNum, limitDen, qty, remaining }
    datumFields: [
      { id: "owner",     label: "owner :: PubKeyHash",       type: "PubKeyHash" },
      { id: "base",      label: "base :: CurrencySymbol",    type: "CurrencySymbol" },
      { id: "quote",     label: "quote :: CurrencySymbol",   type: "CurrencySymbol" },
      { id: "side",      label: "side :: Integer  -- 0=Buy,1=Sell", type: "Integer" },
      { id: "limitNum",  label: "limitNum :: Integer",       type: "Integer" },
      { id: "limitDen",  label: "limitDen :: Integer",       type: "Integer" },
      { id: "qty",       label: "qty :: Integer",            type: "Integer" },
      { id: "remaining", label: "remaining :: Integer",      type: "Integer" }
    ],

    actions: [
      { id: "Fill",   label: "Fill Order" },
      { id: "Cancel", label: "Cancel Order" }
    ],

    constraints: [
      {
        id: "priceMeetsLimit",
        label: "Execution price satisfies limit (≤ for buy, ≥ for sell)"
      },
      {
        id: "remainingDecreases",
        label: "remaining decreases by filled amount"
      },
      {
        id: "noOverfill",
        label: "Cannot fill more than remaining quantity"
      },
      {
        id: "signedByOwnerForCancel",
        label: "Cancel tx signed by order owner"
      }
    ],

    defaultActionConstraints: {
      Fill: [
        "priceMeetsLimit",
        "remainingDecreases",
        "noOverfill"
      ],
      Cancel: [
        "signedByOwnerForCancel"
      ]
    }
  },

  // -------------------------------------------------------------
  // 13) Stop-loss / take-profit vault (conditional exit)
  // -------------------------------------------------------------
  {
    id: "conditionalExitVault",
    label: "Stop-loss / Take-profit Vault",
    datumType: "VaultDatum",
    redeemerType: "VaultAction",

    // VaultDatum { owner, asset, trigger:{Stop|Take}, priceNum/Den, dest, armed }
    datumFields: [
      { id: "owner",      label: "owner :: PubKeyHash",                   type: "PubKeyHash" },
      { id: "asset",      label: "asset :: CurrencySymbol",               type: "CurrencySymbol" },
      { id: "triggerType",label: "triggerType :: Integer  -- 0=Stop,1=Take", type: "Integer" },
      { id: "priceNum",   label: "priceNum :: Integer",                   type: "Integer" },
      { id: "priceDen",   label: "priceDen :: Integer",                   type: "Integer" },
      { id: "dest",       label: "dest :: PubKeyHash",                    type: "PubKeyHash" },
      { id: "armed",      label: "armed :: Integer  -- 0=Off,1=On",       type: "Integer" }
    ],

    actions: [
      { id: "Trigger", label: "Trigger Exit" },
      { id: "Disarm",  label: "Disarm Vault" }
    ],

    constraints: [
      {
        id: "oracleFreshAndSigned",
        label: "Oracle reference input is fresh and properly signed"
      },
      {
        id: "directionSatisfied",
        label: "Price crosses stop/take direction as required"
      },
      {
        id: "sendsAssetsToDest",
        label: "Vault assets sent to dest on trigger"
      },
      {
        id: "onlyTriggerWhenArmed",
        label: "Trigger only allowed when armed = 1"
      },
      {
        id: "signedByOwnerForDisarm",
        label: "Disarm tx signed by owner"
      }
    ],

    defaultActionConstraints: {
      Trigger: [
        "oracleFreshAndSigned",
        "directionSatisfied",
        "sendsAssetsToDest",
        "onlyTriggerWhenArmed"
      ],
      Disarm: [
        "signedByOwnerForDisarm"
      ]
    }
  },

  // -------------------------------------------------------------
  // 14) vAMM perpetuals (virtual reserves)
  // -------------------------------------------------------------
  {
    id: "vammPerps",
    label: "vAMM Perpetuals",
    datumType: "PerpPoolDatum",
    redeemerType: "PerpAction",

    // PerpPoolDatum { vResA, vResB, indexPrice, fundingRate, feeBps, poolNFT }
    datumFields: [
      { id: "vResA",       label: "vResA :: Integer",           type: "Integer" },
      { id: "vResB",       label: "vResB :: Integer",           type: "Integer" },
      { id: "indexPrice",  label: "indexPrice :: Integer",      type: "Integer" },
      { id: "fundingRate", label: "fundingRate :: Integer",     type: "Integer" },
      { id: "feeBps",      label: "feeBps :: Integer",          type: "Integer" },
      { id: "poolNFT",     label: "poolNFT :: CurrencySymbol",  type: "CurrencySymbol" }
    ],

    actions: [
      { id: "Open",         label: "Open Position" },
      { id: "Close",        label: "Close Position" },
      { id: "AdjustMargin", label: "Adjust Margin" },
      { id: "SettleFunding",label: "Settle Funding" }
    ],

    constraints: [
      {
        id: "priceFromVammFormula",
        label: "Trade price derived from vAMM virtual reserves formula"
      },
      {
        id: "positionHealthNonNegative",
        label: "Post-tx position health ≥ 0 (no undercollateralized state)"
      },
      {
        id: "fundingSettlementCorrect",
        label: "Funding settlement updates PnL and indexPrice consistently"
      },
      {
        id: "uniquePerpPoolViaNFT",
        label: "Unique perp pool UTxO enforced via poolNFT"
      }
    ],

    defaultActionConstraints: {
      Open: [
        "uniquePerpPoolViaNFT",
        "priceFromVammFormula",
        "positionHealthNonNegative"
      ],
      Close: [
        "uniquePerpPoolViaNFT",
        "priceFromVammFormula",
        "fundingSettlementCorrect"
      ],
      AdjustMargin: [
        "uniquePerpPoolViaNFT",
        "positionHealthNonNegative"
      ],
      SettleFunding: [
        "uniquePerpPoolViaNFT",
        "fundingSettlementCorrect"
      ]
    }
  },

  // -------------------------------------------------------------
  // 15) Margin account (isolation + health factor)
  // -------------------------------------------------------------
  {
    id: "marginAccount",
    label: "Margin Account (Health Factor)",
    datumType: "MarginAccountDatum",
    redeemerType: "MarginAccountAction",

    // Account NFT + Datum { owner, collaterals[], borrows[], healthFactors }
    // Vector details handled off-chain; store aggregate values.
    datumFields: [
      { id: "owner",          label: "owner :: PubKeyHash",       type: "PubKeyHash" },
      { id: "collateralValue",label: "collateralValue :: Integer",type: "Integer" },
      { id: "borrowValue",    label: "borrowValue :: Integer",    type: "Integer" },
      { id: "healthFactor",   label: "healthFactor :: Integer",   type: "Integer" }
    ],

    actions: [
      { id: "Deposit",   label: "Deposit Collateral" },
      { id: "Withdraw",  label: "Withdraw Collateral" },
      { id: "Borrow",    label: "Borrow Asset" },
      { id: "Repay",     label: "Repay Borrow" },
      { id: "Liquidate", label: "Liquidate Account" }
    ],

    constraints: [
      {
        id: "postHealthAboveOne",
        label: "Post-tx health factor ≥ 1 for user actions"
      },
      {
        id: "onlyLiquidateIfBelowThreshold",
        label: "Liquidation allowed only when HF below threshold"
      },
      {
        id: "liquidationBonusBounded",
        label: "Liquidation bonus within protocol-defined bounds"
      }
    ],

    defaultActionConstraints: {
      Deposit: [
        "postHealthAboveOne"
      ],
      Withdraw: [
        "postHealthAboveOne"
      ],
      Borrow: [
        "postHealthAboveOne"
      ],
      Repay: [
        "postHealthAboveOne"
      ],
      Liquidate: [
        "onlyLiquidateIfBelowThreshold",
        "liquidationBonusBounded"
      ]
    }
  },

  // -------------------------------------------------------------
  // 16) Options AMM (covered calls/puts market)
  // -------------------------------------------------------------
  {
    id: "optionsAmm",
    label: "Options AMM (Covered Options)",
    datumType: "OptionsPoolDatum",
    redeemerType: "OptionsPoolAction",

    // Combined view of Series + Pool for UI/template purposes
    // { underlier, strike, expiry, optType, collateralReq, resOption, resCollateral, feeBps, pricingParams }
    datumFields: [
      { id: "underlier",     label: "underlier :: CurrencySymbol", type: "CurrencySymbol" },
      { id: "strike",        label: "strike :: Integer",           type: "Integer" },
      { id: "expiry",        label: "expiry :: POSIXTime",         type: "POSIXTime" },
      { id: "optType",       label: "optType :: Integer  -- 0=Call,1=Put", type: "Integer" },
      { id: "collateralReq", label: "collateralReq :: Integer",    type: "Integer" },
      { id: "resOption",     label: "resOption :: Integer",        type: "Integer" },
      { id: "resCollateral", label: "resCollateral :: Integer",    type: "Integer" },
      { id: "feeBps",        label: "feeBps :: Integer",           type: "Integer" },
      { id: "pricingParams", label: "pricingParams :: Integer",    type: "Integer" }
    ],

    actions: [
      { id: "Swap",      label: "Swap Option" },
      { id: "AddLiq",    label: "Add Liquidity" },
      { id: "RemoveLiq", label: "Remove Liquidity" },
      { id: "Exercise",  label: "Exercise Option" }
    ],

    constraints: [
      {
        id: "exerciseOnlyWhenAllowed",
        label: "Exercise respects European/American exercise rules"
      },
      {
        id: "collateralSufficient",
        label: "Pool collateral sufficient to honor exercised options"
      },
      {
        id: "pricingInvariantRespected",
        label: "Swaps respect the options AMM pricing invariant/curve"
      }
    ],

    defaultActionConstraints: {
      Swap: [
        "pricingInvariantRespected",
        "collateralSufficient"
      ],
      AddLiq: [
        "collateralSufficient"
      ],
      RemoveLiq: [
        "collateralSufficient"
      ],
      Exercise: [
        "exerciseOnlyWhenAllowed",
        "collateralSufficient"
      ]
    }
  },

  // -------------------------------------------------------------
  // 17) Covered-call vault (auto-writing, rolling)
  // -------------------------------------------------------------
  {
    id: "coveredCallVault",
    label: "Covered-call Vault",
    datumType: "CoveredCallVaultDatum",
    redeemerType: "CoveredCallVaultAction",

    // VaultDatum { underlier, strike, expiry, sharesSupply, queuedPremium, round }
    datumFields: [
      { id: "underlier",     label: "underlier :: CurrencySymbol", type: "CurrencySymbol" },
      { id: "strike",        label: "strike :: Integer",           type: "Integer" },
      { id: "expiry",        label: "expiry :: POSIXTime",         type: "POSIXTime" },
      { id: "sharesSupply",  label: "sharesSupply :: Integer",     type: "Integer" },
      { id: "queuedPremium", label: "queuedPremium :: Integer",    type: "Integer" },
      { id: "round",         label: "round :: Integer",            type: "Integer" }
    ],

    actions: [
      { id: "Deposit", label: "Deposit Underlier" },
      { id: "Withdraw",label: "Withdraw Shares" },
      { id: "Write",   label: "Write Calls for Round" },
      { id: "Settle",  label: "Settle Round" }
    ],

    constraints: [
      {
        id: "writesOnlyInCurrentRound",
        label: "Write operations only within active round"
      },
      {
        id: "settleAfterExpiry",
        label: "Settlement only after expiry of round"
      },
      {
        id: "premiumsDistributedProRata",
        label: "Premiums distributed pro-rata to share holders"
      },
      {
        id: "shareAccountingCorrect",
        label: "Share mint/burn consistent with deposits/withdrawals"
      }
    ],

    defaultActionConstraints: {
      Deposit: [
        "shareAccountingCorrect"
      ],
      Withdraw: [
        "shareAccountingCorrect"
      ],
      Write: [
        "writesOnlyInCurrentRound",
        "shareAccountingCorrect"
      ],
      Settle: [
        "settleAfterExpiry",
        "premiumsDistributedProRata",
        "shareAccountingCorrect"
      ]
    }
  },

  // -------------------------------------------------------------
  // 18) Options settlement escrow (European expiry)
  // -------------------------------------------------------------
  {
    id: "optionsSettlementEscrow",
    label: "Options Settlement Escrow (European)",
    datumType: "OptionEscrowDatum",
    redeemerType: "OptionEscrowAction",

    // Combined SeriesDatum + EscrowDatum view for UI:
    // { holder, qty, strike, expiry, underlier, optionToken }
    datumFields: [
      { id: "holder",     label: "holder :: PubKeyHash",        type: "PubKeyHash" },
      { id: "qty",        label: "qty :: Integer",              type: "Integer" },
      { id: "strike",     label: "strike :: Integer",           type: "Integer" },
      { id: "expiry",     label: "expiry :: POSIXTime",         type: "POSIXTime" },
      { id: "underlier",  label: "underlier :: CurrencySymbol", type: "CurrencySymbol" },
      { id: "optionToken",label: "optionToken :: CurrencySymbol",type: "CurrencySymbol" }
    ],

    actions: [
      { id: "Exercise", label: "Exercise" },
      { id: "Refund",   label: "Refund After Grace" }
    ],

    constraints: [
      {
        id: "withinExerciseWindow",
        label: "Exercise only allowed in [expiry, expiry+grace]"
      },
      {
        id: "afterGraceForRefund",
        label: "Refund only allowed after expiry+grace"
      },
      {
        id: "paysIntrinsicValueCorrectly",
        label: "Settlement pays max(0, intrinsic value) to holder"
      }
    ],

    defaultActionConstraints: {
      Exercise: [
        "withinExerciseWindow",
        "paysIntrinsicValueCorrectly"
      ],
      Refund: [
        "afterGraceForRefund"
      ]
    }
  },

  // -------------------------------------------------------------
  // 19) Bonding-curve token sale (mint price = f(supply))
  // -------------------------------------------------------------
  {
    id: "bondingCurveSale",
    label: "Bonding-curve Token Sale",
    datumType: "CurveSaleDatum",
    redeemerType: "CurveSaleAction",

    // CurveSaleDatum { supply, reserve }
    // Curve params (k, exponent, etc.) via reference input.
    datumFields: [
      { id: "supply",  label: "supply :: Integer",  type: "Integer" },
      { id: "reserve", label: "reserve :: Integer", type: "Integer" }
    ],

    actions: [
      { id: "Buy",  label: "Buy from Curve" },
      { id: "Sell", label: "Sell to Curve" }
    ],

    constraints: [
      {
        id: "usesCurveParamsRef",
        label: "Uses curve params reference input (k, exponent, etc.)"
      },
      {
        id: "paidAccordingToCurve",
        label: "Payment vs minted/burned amount matches ∫f(s) ds"
      },
      {
        id: "supplyUpdatedCorrectly",
        label: "Token supply updated consistently with buy/sell Δ"
      },
      {
        id: "feeSkimApplied",
        label: "Protocol/treasury fee skim applied as configured"
      }
    ],

    defaultActionConstraints: {
      Buy: [
        "usesCurveParamsRef",
        "paidAccordingToCurve",
        "supplyUpdatedCorrectly",
        "feeSkimApplied"
      ],
      Sell: [
        "usesCurveParamsRef",
        "paidAccordingToCurve",
        "supplyUpdatedCorrectly",
        "feeSkimApplied"
      ]
    }
  },

  // -------------------------------------------------------------
  // 20) Dutch auction (decaying price)
  // -------------------------------------------------------------
  {
    id: "dutchAuction",
    label: "Dutch Auction (Decaying Price)",
    datumType: "DutchAuctionDatum",
    redeemerType: "DutchAuctionAction",

    // AuctionDatum { seller, asset, start, duration, startPrice, floorPrice, taken }
    datumFields: [
      { id: "seller",     label: "seller :: PubKeyHash",        type: "PubKeyHash" },
      { id: "asset",      label: "asset :: CurrencySymbol",     type: "CurrencySymbol" },
      { id: "start",      label: "start :: POSIXTime",          type: "POSIXTime" },
      { id: "duration",   label: "duration :: Integer",         type: "Integer" },
      { id: "startPrice", label: "startPrice :: Integer",       type: "Integer" },
      { id: "floorPrice", label: "floorPrice :: Integer",       type: "Integer" },
      { id: "taken",      label: "taken :: Integer  -- 0=No,1=Yes", type: "Integer" }
    ],

    actions: [
      { id: "Buy",    label: "Buy at Current Price" },
      { id: "Cancel", label: "Cancel Auction" }
    ],

    constraints: [
      {
        id: "currentPriceComputedCorrectly",
        label: "Current price computed as function of time between start and floor"
      },
      {
        id: "payAtLeastCurrentPrice",
        label: "Buyer pays at least the current auction price"
      },
      {
        id: "singleFillUnlessMultiUnit",
        label: "Auction cannot be double-filled when taken = 1"
      },
      {
        id: "signedBySellerForCancel",
        label: "Cancel tx signed by seller"
      }
    ],

    defaultActionConstraints: {
      Buy: [
        "currentPriceComputedCorrectly",
        "payAtLeastCurrentPrice",
        "singleFillUnlessMultiUnit"
      ],
      Cancel: [
        "signedBySellerForCancel"
      ]
    }
  },
    // -------------------------------------------------------------
  // 21) English auction (highest-bid-wins)
  // -------------------------------------------------------------
  {
    id: "englishAuction",
    label: "English Auction (Highest Bid Wins)",
    datumType: "EnglishAuctionDatum",
    redeemerType: "EnglishAuctionAction",

    // AuctionDatum { seller, asset, minBid, deadline, topBid, topBidder, bidBondBps }
    datumFields: [
      { id: "seller",     label: "seller :: PubKeyHash",        type: "PubKeyHash" },
      { id: "asset",      label: "asset :: CurrencySymbol",     type: "CurrencySymbol" },
      { id: "minBid",     label: "minBid :: Integer",           type: "Integer" },
      { id: "deadline",   label: "deadline :: POSIXTime",       type: "POSIXTime" },
      { id: "topBid",     label: "topBid :: Integer",           type: "Integer" },
      { id: "topBidder",  label: "topBidder :: PubKeyHash",     type: "PubKeyHash" },
      { id: "bidBondBps", label: "bidBondBps :: Integer",       type: "Integer" }
    ],

    actions: [
      { id: "Bid",    label: "Place Bid" },
      { id: "Settle", label: "Settle Auction" },
      { id: "Cancel", label: "Cancel Auction" }
    ],

    constraints: [
      {
        id: "bidAboveMinAndTop",
        label: "New bid > max(minBid, topBid + min increment)"
      },
      {
        id: "bidsBeforeDeadline",
        label: "Bids only allowed before deadline"
      },
      {
        id: "settleAfterDeadline",
        label: "Settle only after deadline"
      },
      {
        id: "winnerPaysTopBid",
        label: "Winner pays highest valid bid; asset sent to winner"
      },
      {
        id: "refundLosingBonds",
        label: "Non-winning bonds refunded correctly"
      },
      {
        id: "cancelOnlyIfNoBids",
        label: "Cancel allowed only if there are no valid bids"
      },
      {
        id: "signedBySellerForCancel",
        label: "Cancel tx signed by seller"
      }
    ],

    defaultActionConstraints: {
      Bid: [
        "bidAboveMinAndTop",
        "bidsBeforeDeadline"
      ],
      Settle: [
        "settleAfterDeadline",
        "winnerPaysTopBid",
        "refundLosingBonds"
      ],
      Cancel: [
        "cancelOnlyIfNoBids",
        "signedBySellerForCancel"
      ]
    }
  },

  // -------------------------------------------------------------
  // 22) Sealed-bid commit–reveal auction
  // -------------------------------------------------------------
  {
    id: "sealedBidAuction",
    label: "Sealed-bid Commit–Reveal Auction",
    datumType: "CommitDatum",
    redeemerType: "CommitAction",

    // CommitDatum { bidder, commitHash, deadlineReveal }
    datumFields: [
      { id: "bidder",       label: "bidder :: PubKeyHash",        type: "PubKeyHash" },
      { id: "commitHash",   label: "commitHash :: TokenName",     type: "TokenName" },
      { id: "deadlineReveal",label: "deadlineReveal :: POSIXTime",type: "POSIXTime" }
    ],

    actions: [
      { id: "Reveal", label: "Reveal Bid" },
      { id: "Refund", label: "Refund After Reveal Window" }
    ],

    constraints: [
      {
        id: "commitmentMatchesReveal",
        label: "commitHash matches hash(amount || salt)"
      },
      {
        id: "revealBeforeDeadline",
        label: "Reveal only allowed before deadlineReveal"
      },
      {
        id: "refundAfterDeadline",
        label: "Refund only after reveal deadline"
      },
      {
        id: "highestRevealedWins",
        label: "Settlement selects highest revealed valid bid"
      }
    ],

    defaultActionConstraints: {
      Reveal: [
        "commitmentMatchesReveal",
        "revealBeforeDeadline"
      ],
      Refund: [
        "refundAfterDeadline"
      ]
    }
  },

  // -------------------------------------------------------------
  // 23) Launchpad / IDO with vesting & cliffs
  // -------------------------------------------------------------
  {
    id: "launchpadIdo",
    label: "Launchpad / IDO with Vesting",
    datumType: "VestingDatum",
    redeemerType: "VestingAction",

    // VestingDatum { beneficiary, total, start, cliff, end, claimed }
    datumFields: [
      { id: "beneficiary", label: "beneficiary :: PubKeyHash", type: "PubKeyHash" },
      { id: "total",       label: "total :: Integer",          type: "Integer" },
      { id: "start",       label: "start :: POSIXTime",        type: "POSIXTime" },
      { id: "cliff",       label: "cliff :: POSIXTime",        type: "POSIXTime" },
      { id: "end",         label: "end :: POSIXTime",          type: "POSIXTime" },
      { id: "claimed",     label: "claimed :: Integer",        type: "Integer" }
    ],

    actions: [
      { id: "Buy",   label: "Buy Allocation" },
      { id: "Claim", label: "Claim Vested Tokens" },
      { id: "Refund",label: "Refund Contribution" }
    ],

    constraints: [
      {
        id: "usesSaleParamsRef",
        label: "Reads SaleParams reference input (price, cap, window, whitelist)"
      },
      {
        id: "buysWithinWindow",
        label: "Buys only within [start, end] sale window"
      },
      {
        id: "capNotExceeded",
        label: "Total contributions do not exceed hard cap"
      },
      {
        id: "claimsLinearAfterCliff",
        label: "Claimable amount grows linearly between cliff and end"
      },
      {
        id: "refundIfSoftCapUnmet",
        label: "Refunds enabled if soft cap not reached"
      }
    ],

    defaultActionConstraints: {
      Buy: [
        "usesSaleParamsRef",
        "buysWithinWindow",
        "capNotExceeded"
      ],
      Claim: [
        "usesSaleParamsRef",
        "claimsLinearAfterCliff"
      ],
      Refund: [
        "usesSaleParamsRef",
        "refundIfSoftCapUnmet"
      ]
    }
  },

  // -------------------------------------------------------------
  // 24) Streaming payments / payroll (linear vest)
  // -------------------------------------------------------------
  {
    id: "streamingPayments",
    label: "Streaming Payments / Payroll",
    datumType: "StreamDatum",
    redeemerType: "StreamAction",

    // StreamDatum { sender, recipient, ratePerSec, start, end, claimed }
    datumFields: [
      { id: "sender",     label: "sender :: PubKeyHash",        type: "PubKeyHash" },
      { id: "recipient",  label: "recipient :: PubKeyHash",     type: "PubKeyHash" },
      { id: "ratePerSec", label: "ratePerSec :: Integer",       type: "Integer" },
      { id: "start",      label: "start :: POSIXTime",          type: "POSIXTime" },
      { id: "end",        label: "end :: POSIXTime",            type: "POSIXTime" },
      { id: "claimed",    label: "claimed :: Integer",          type: "Integer" }
    ],

    actions: [
      { id: "Claim",  label: "Claim Stream" },
      { id: "Cancel", label: "Cancel Stream" }
    ],

    constraints: [
      {
        id: "claimableMatchesFormula",
        label: "claimable = ratePerSec * elapsed - claimed, bounded by deposit"
      },
      {
        id: "noOverclaim",
        label: "Cannot claim more than accrued amount"
      },
      {
        id: "signedBySenderForCancel",
        label: "Cancel tx signed by sender (if allowed)"
      }
    ],

    defaultActionConstraints: {
      Claim: [
        "claimableMatchesFormula",
        "noOverclaim"
      ],
      Cancel: [
        "signedBySenderForCancel"
      ]
    }
  },

  // -------------------------------------------------------------
  // 25) Subscription pull-payments (allowlist + limits)
  // -------------------------------------------------------------
  {
    id: "subscriptionPull",
    label: "Subscription Pull-payments",
    datumType: "SubscriptionDatum",
    redeemerType: "SubscriptionAction",

    // SubDatum { subscriber, merchant, period, limit, spentInPeriod, resetAt }
    datumFields: [
      { id: "subscriber",    label: "subscriber :: PubKeyHash",   type: "PubKeyHash" },
      { id: "merchant",      label: "merchant :: PubKeyHash",     type: "PubKeyHash" },
      { id: "period",        label: "period :: Integer",          type: "Integer" },
      { id: "limit",         label: "limit :: Integer",           type: "Integer" },
      { id: "spentInPeriod", label: "spentInPeriod :: Integer",   type: "Integer" },
      { id: "resetAt",       label: "resetAt :: POSIXTime",       type: "POSIXTime" }
    ],

    actions: [
      { id: "Charge", label: "Charge Subscriber" },
      { id: "Cancel", label: "Cancel Subscription" },
      { id: "TopUp",  label: "Top Up Escrow" },
      { id: "Update", label: "Update Plan" }
    ],

    constraints: [
      {
        id: "chargeWithinRemainingLimit",
        label: "Charge amount ≤ (limit - spentInPeriod)"
      },
      {
        id: "periodResetLogicCorrect",
        label: "spentInPeriod resets correctly at resetAt"
      },
      {
        id: "updatesRequireSubscriberSig",
        label: "Plan updates require subscriber signature"
      },
      {
        id: "topUpBySubscriberOnly",
        label: "Top-ups must be funded by subscriber"
      }
    ],

    defaultActionConstraints: {
      Charge: [
        "chargeWithinRemainingLimit",
        "periodResetLogicCorrect"
      ],
      Cancel: [
        "updatesRequireSubscriberSig"
      ],
      TopUp: [
        "topUpBySubscriberOnly"
      ],
      Update: [
        "updatesRequireSubscriberSig"
      ]
    }
  },

  // -------------------------------------------------------------
  // 26) Escrow with dispute / arbiter (buyer–seller)
  // -------------------------------------------------------------
  {
    id: "disputeEscrow",
    label: "Escrow with Dispute / Arbiter",
    datumType: "DisputeEscrowDatum",
    redeemerType: "DisputeEscrowAction",

    // EscrowDatum { buyer, seller, arbiter, state, deadline }
    datumFields: [
      { id: "buyer",    label: "buyer :: PubKeyHash",    type: "PubKeyHash" },
      { id: "seller",   label: "seller :: PubKeyHash",   type: "PubKeyHash" },
      { id: "arbiter",  label: "arbiter :: PubKeyHash",  type: "PubKeyHash" },
      { id: "state",    label: "state :: Integer",       type: "Integer" },
      { id: "deadline", label: "deadline :: POSIXTime",  type: "POSIXTime" }
    ],

    actions: [
      { id: "Release",   label: "Release to Seller" },
      { id: "Refund",    label: "Refund to Buyer" },
      { id: "Arbitrate", label: "Arbitrate Outcome" }
    ],

    constraints: [
      {
        id: "mutualReleaseRequiresBothSigs",
        label: "Mutual Release requires both buyer and seller signatures"
      },
      {
        id: "arbiterAfterDeadline",
        label: "Arbiter can resolve only after deadline"
      },
      {
        id: "singleResolution",
        label: "Escrow can be resolved only once"
      }
    ],

    defaultActionConstraints: {
      Release: [
        "mutualReleaseRequiresBothSigs",
        "singleResolution"
      ],
      Refund: [
        "mutualReleaseRequiresBothSigs",
        "singleResolution"
      ],
      Arbitrate: [
        "arbiterAfterDeadline",
        "singleResolution"
      ]
    }
  },

  // -------------------------------------------------------------
  // 27) Milestone escrow / crowdfunding (refund conditions)
  // -------------------------------------------------------------
  {
    id: "milestoneEscrow",
    label: "Milestone Escrow / Crowdfunding",
    datumType: "CampaignDatum",
    redeemerType: "CampaignAction",

    // CampaignDatum { creator, target, deadline, raised, milestoneIdx }
    datumFields: [
      { id: "creator",     label: "creator :: PubKeyHash",   type: "PubKeyHash" },
      { id: "target",      label: "target :: Integer",       type: "Integer" },
      { id: "deadline",    label: "deadline :: POSIXTime",   type: "POSIXTime" },
      { id: "raised",      label: "raised :: Integer",       type: "Integer" },
      { id: "milestoneIdx",label: "milestoneIdx :: Integer", type: "Integer" }
    ],

    actions: [
      { id: "Pledge",         label: "Pledge Funds" },
      { id: "UnlockMilestone",label: "Unlock Milestone" },
      { id: "Refund",         label: "Refund Pledges" }
    ],

    constraints: [
      {
        id: "pledgeBeforeDeadline",
        label: "Pledges only allowed before deadline"
      },
      {
        id: "unlockOnlyIfTargetMet",
        label: "Milestones only unlock if target is met"
      },
      {
        id: "milestoneProofByAttestor",
        label: "Milestone unlocks require attestor/oracle proof"
      },
      {
        id: "refundAfterFailure",
        label: "Refunds only after deadline if target not met"
      }
    ],

    defaultActionConstraints: {
      Pledge: [
        "pledgeBeforeDeadline"
      ],
      UnlockMilestone: [
        "unlockOnlyIfTargetMet",
        "milestoneProofByAttestor"
      ],
      Refund: [
        "refundAfterFailure"
      ]
    }
  },

  // -------------------------------------------------------------
  // 28) CDP lending vault (over-collateralized)
  // -------------------------------------------------------------
  {
    id: "cdpVault",
    label: "CDP Lending Vault",
    datumType: "CdpVaultDatum",
    redeemerType: "CdpVaultAction",

    // VaultDatum { owner, collValue, debt, MCR, LCR }
    datumFields: [
      { id: "owner",     label: "owner :: PubKeyHash",  type: "PubKeyHash" },
      { id: "collValue", label: "collValue :: Integer", type: "Integer" },
      { id: "debt",      label: "debt :: Integer",      type: "Integer" },
      { id: "MCR",       label: "MCR :: Integer",       type: "Integer" },
      { id: "LCR",       label: "LCR :: Integer",       type: "Integer" }
    ],

    actions: [
      { id: "Open",       label: "Open Vault" },
      { id: "Draw",       label: "Draw Stable" },
      { id: "Repay",      label: "Repay Stable" },
      { id: "Close",      label: "Close Vault" },
      { id: "Liquidate",  label: "Liquidate Vault" }
    ],

    constraints: [
      {
        id: "postOpMeetsMCR",
        label: "Post-operation collateral ratio ≥ MCR"
      },
      {
        id: "liquidationIfBelowLCR",
        label: "Liquidation allowed if ratio < LCR"
      },
      {
        id: "oracleFreshAndSigned",
        label: "Oracle ref fresh and signed for price checks"
      }
    ],

    defaultActionConstraints: {
      Open: [
        "oracleFreshAndSigned"
      ],
      Draw: [
        "oracleFreshAndSigned",
        "postOpMeetsMCR"
      ],
      Repay: [
        "oracleFreshAndSigned"
      ],
      Close: [
        "oracleFreshAndSigned",
        "postOpMeetsMCR"
      ],
      Liquidate: [
        "oracleFreshAndSigned",
        "liquidationIfBelowLCR"
      ]
    }
  },

  // -------------------------------------------------------------
  // 29) Pooled lending (lender shares + interest accrual)
  // -------------------------------------------------------------
  {
    id: "pooledLending",
    label: "Pooled Lending (Variable Rate)",
    datumType: "LendingPoolDatum",
    redeemerType: "LendingPoolAction",

    // PoolDatum { cash, borrows, reserveFactor, rateModelParams }
    datumFields: [
      { id: "cash",           label: "cash :: Integer",           type: "Integer" },
      { id: "borrows",        label: "borrows :: Integer",        type: "Integer" },
      { id: "reserveFactor",  label: "reserveFactor :: Integer",  type: "Integer" },
      { id: "rateModelParams",label: "rateModelParams :: Integer",type: "Integer" }
    ],

    actions: [
      { id: "Deposit", label: "Deposit" },
      { id: "Withdraw",label: "Withdraw" },
      { id: "Borrow",  label: "Borrow" },
      { id: "Repay",   label: "Repay" },
      { id: "Accrue",  label: "Accrue Interest" }
    ],

    constraints: [
      {
        id: "exchangeRateMonotonic",
        label: "Share exchange rate increases with interest accrual"
      },
      {
        id: "accrualUpdatesBorrowsAndReserves",
        label: "Accrue updates borrows and reserves correctly"
      },
      {
        id: "borrowWithinUtilizationLimits",
        label: "Borrowing constrained by utilization-based limits"
      }
    ],

    defaultActionConstraints: {
      Deposit: [
        "exchangeRateMonotonic"
      ],
      Withdraw: [
        "exchangeRateMonotonic"
      ],
      Borrow: [
        "borrowWithinUtilizationLimits",
        "exchangeRateMonotonic"
      ],
      Repay: [
        "exchangeRateMonotonic"
      ],
      Accrue: [
        "accrualUpdatesBorrowsAndReserves",
        "exchangeRateMonotonic"
      ]
    }
  },

  // -------------------------------------------------------------
  // 30) Revolving credit line (rate model + utilization)
  // -------------------------------------------------------------
  {
    id: "revolvingCreditLine",
    label: "Revolving Credit Line",
    datumType: "CreditLineDatum",
    redeemerType: "CreditLineAction",

    // LineDatum { borrower, limit, drawn, rateModel, lastAccrual }
    datumFields: [
      { id: "borrower",   label: "borrower :: PubKeyHash", type: "PubKeyHash" },
      { id: "limit",      label: "limit :: Integer",       type: "Integer" },
      { id: "drawn",      label: "drawn :: Integer",       type: "Integer" },
      { id: "rateModel",  label: "rateModel :: Integer",   type: "Integer" },
      { id: "lastAccrual",label: "lastAccrual :: POSIXTime", type: "POSIXTime" }
    ],

    actions: [
      { id: "Draw",        label: "Draw" },
      { id: "Repay",       label: "Repay" },
      { id: "AdjustLimit", label: "Adjust Limit" },
      { id: "Accrue",      label: "Accrue Interest" }
    ],

    constraints: [
      {
        id: "drawnNotExceedLimit",
        label: "Draw cannot exceed credit limit"
      },
      {
        id: "interestAccrualCorrect",
        label: "Interest accrual applied correctly to drawn amount"
      },
      {
        id: "limitChangesRequireControllerSig",
        label: "Limit changes require controller/governance signature"
      }
    ],

    defaultActionConstraints: {
      Draw: [
        "drawnNotExceedLimit"
      ],
      Repay: [
        "interestAccrualCorrect"
      ],
      AdjustLimit: [
        "limitChangesRequireControllerSig"
      ],
      Accrue: [
        "interestAccrualCorrect"
      ]
    }
  },

  // -------------------------------------------------------------
  // 31) Liquidation auction (surplus/deficit settlement)
  // -------------------------------------------------------------
  {
    id: "liquidationAuction",
    label: "Liquidation Auction (Surplus/Deficit)",
    datumType: "LiquidationAuctionDatum",
    redeemerType: "LiquidationAuctionAction",

    // AuctionDatum { lot, quote, start, end, minInc, topBid, topBidder, mode, kicker }
    datumFields: [
      { id: "lot",      label: "lot :: CurrencySymbol",     type: "CurrencySymbol" },
      { id: "quote",    label: "quote :: CurrencySymbol",   type: "CurrencySymbol" },
      { id: "start",    label: "start :: POSIXTime",        type: "POSIXTime" },
      { id: "end",      label: "end :: POSIXTime",          type: "POSIXTime" },
      { id: "minInc",   label: "minInc :: Integer",         type: "Integer" },
      { id: "topBid",   label: "topBid :: Integer",         type: "Integer" },
      { id: "topBidder",label: "topBidder :: PubKeyHash",   type: "PubKeyHash" },
      { id: "mode",     label: "mode :: Integer  -- 0=Surplus,1=Deficit", type: "Integer" },
      { id: "kicker",   label: "kicker :: PubKeyHash",      type: "PubKeyHash" }
    ],

    actions: [
      { id: "Bid",    label: "Place Bid" },
      { id: "Settle", label: "Settle Auction" },
      { id: "Cancel", label: "Cancel Auction" }
    ],

    constraints: [
      {
        id: "validBidIncrement",
        label: "New bid ≥ topBid + minInc"
      },
      {
        id: "settleOnlyAfterEnd",
        label: "Settle only allowed after end time"
      },
      {
        id: "singleSettlement",
        label: "Auction can be settled only once"
      }
    ],

    defaultActionConstraints: {
      Bid: [
        "validBidIncrement"
      ],
      Settle: [
        "settleOnlyAfterEnd",
        "singleSettlement"
      ],
      Cancel: [
        "singleSettlement"
      ]
    }
  },

  // -------------------------------------------------------------
  // 32) Over-collateralized stablecoin (DAI-like)
  // -------------------------------------------------------------
  {
    id: "stablecoinVaults",
    label: "Over-collateralized Stablecoin",
    datumType: "StableVaultDatum",
    redeemerType: "StableVaultAction",

    // VaultDatum { owner, coll, debt, MCR, rateIdx }
    datumFields: [
      { id: "owner",  label: "owner :: PubKeyHash",  type: "PubKeyHash" },
      { id: "coll",   label: "coll :: Integer",      type: "Integer" },
      { id: "debt",   label: "debt :: Integer",      type: "Integer" },
      { id: "MCR",    label: "MCR :: Integer",       type: "Integer" },
      { id: "rateIdx",label: "rateIdx :: Integer",   type: "Integer" }
    ],

    actions: [
      { id: "Open",      label: "Open Vault" },
      { id: "Draw",      label: "Draw Stable" },
      { id: "Repay",     label: "Repay Stable" },
      { id: "Liquidate", label: "Liquidate Vault" }
    ],

    constraints: [
      {
        id: "postOpMeetsMCRStable",
        label: "Post-operation coll/debt ratio ≥ MCR"
      },
      {
        id: "rateAccrualFromRateRef",
        label: "Stability fee accrual uses RateRef input"
      },
      {
        id: "liquidationIfBelowMCR",
        label: "Liquidation allowed if ratio below MCR threshold"
      }
    ],

    defaultActionConstraints: {
      Open: [
        "rateAccrualFromRateRef"
      ],
      Draw: [
        "rateAccrualFromRateRef",
        "postOpMeetsMCRStable"
      ],
      Repay: [
        "rateAccrualFromRateRef"
      ],
      Liquidate: [
        "rateAccrualFromRateRef",
        "liquidationIfBelowMCR"
      ]
    }
  },

  // -------------------------------------------------------------
  // 33) Synthetic assets (mint vs oracle index)
  // -------------------------------------------------------------
  {
    id: "syntheticAssets",
    label: "Synthetic Assets (Oracle-tracking)",
    datumType: "SynthVaultDatum",
    redeemerType: "SynthVaultAction",

    // SynthVault { owner, coll, debt, ratio }
    datumFields: [
      { id: "owner", label: "owner :: PubKeyHash",  type: "PubKeyHash" },
      { id: "coll",  label: "coll :: Integer",      type: "Integer" },
      { id: "debt",  label: "debt :: Integer",      type: "Integer" },
      { id: "ratio", label: "ratio :: Integer",     type: "Integer" }
    ],

    actions: [
      { id: "MintSynth",  label: "Mint sASSET" },
      { id: "BurnSynth",  label: "Burn sASSET" },
      { id: "Liquidate",  label: "Liquidate Vault" }
    ],

    constraints: [
      {
        id: "usesOracleRefFreshSigned",
        label: "Uses fresh, signed oracle reference input"
      },
      {
        id: "maintainsMinCollateralRatio",
        label: "Post-op collateral ratio stays above required threshold"
      }
    ],

    defaultActionConstraints: {
      MintSynth: [
        "usesOracleRefFreshSigned",
        "maintainsMinCollateralRatio"
      ],
      BurnSynth: [
        "usesOracleRefFreshSigned"
      ],
      Liquidate: [
        "usesOracleRefFreshSigned"
      ]
    }
  },

  // -------------------------------------------------------------
  // 34) Rebase token policy (supply rebasing controller)
  // -------------------------------------------------------------
  {
    id: "rebaseTokenPolicy",
    label: "Rebase Token Policy",
    datumType: "RebasePolicyDatum",
    redeemerType: "RebasePolicyAction",

    // Policy usually has no datum; keep empty for UI
    datumFields: [],

    actions: [
      { id: "Rebase", label: "Rebase Supply" }
    ],

    constraints: [
      {
        id: "rebaseOnlyWithControllerRef",
        label: "Rebase requires spending ControllerRef UTxO"
      },
      {
        id: "factorWithinBounds",
        label: "Rebase factor within configured bounds"
      },
      {
        id: "governorSigned",
        label: "ControllerRef update signed by governors"
      }
    ],

    defaultActionConstraints: {
      Rebase: [
        "rebaseOnlyWithControllerRef",
        "factorWithinBounds",
        "governorSigned"
      ]
    }
  },

  // -------------------------------------------------------------
  // 35) Liquid staking derivative wrapper (ADA→sADA)
  // -------------------------------------------------------------
  {
    id: "liquidStakingDerivative",
    label: "Liquid Staking Derivative (ADA → sADA)",
    datumType: "LsdPoolDatum",
    redeemerType: "LsdPoolAction",

    // LSDPool { totalADA, totalShares, fee }
    datumFields: [
      { id: "totalADA",    label: "totalADA :: Integer",    type: "Integer" },
      { id: "totalShares", label: "totalShares :: Integer", type: "Integer" },
      { id: "feeBps",      label: "feeBps :: Integer",      type: "Integer" }
    ],

    actions: [
      { id: "Deposit",         label: "Deposit ADA" },
      { id: "RequestWithdraw", label: "Request Withdraw" },
      { id: "FinalizeWithdraw",label: "Finalize Withdraw" }
    ],

    constraints: [
      {
        id: "exchangeRateCorrect",
        label: "Share exchange rate = totalADA / totalShares (with fees)"
      },
      {
        id: "finalizeAfterEpochDelay",
        label: "Withdrawals only finalized after epoch/queue delay"
      }
    ],

    defaultActionConstraints: {
      Deposit: [
        "exchangeRateCorrect"
      ],
      RequestWithdraw: [
        "exchangeRateCorrect"
      ],
      FinalizeWithdraw: [
        "exchangeRateCorrect",
        "finalizeAfterEpochDelay"
      ]
    }
  },

  // -------------------------------------------------------------
  // 36) Oracle publisher (signed price UTxO with TTL)
  // -------------------------------------------------------------
  {
    id: "oraclePublisher",
    label: "Oracle Publisher",
    datumType: "OracleDatum",
    redeemerType: "OracleAction",

    // OracleDatum { pair, priceNum, priceDen, validUntil, signerPKH }
    datumFields: [
      { id: "pair",       label: "pair :: CurrencySymbol", type: "CurrencySymbol" },
      { id: "priceNum",   label: "priceNum :: Integer",    type: "Integer" },
      { id: "priceDen",   label: "priceDen :: Integer",    type: "Integer" },
      { id: "validUntil", label: "validUntil :: POSIXTime",type: "POSIXTime" },
      { id: "signerPKH",  label: "signerPKH :: PubKeyHash",type: "PubKeyHash" }
    ],

    actions: [
      { id: "Update", label: "Update Price" }
    ],

    constraints: [
      {
        id: "updateSignedBySigner",
        label: "Update tx signed by signerPKH"
      },
      {
        id: "validUntilInFuture",
        label: "validUntil is in the future at publish time"
      }
    ],

    defaultActionConstraints: {
      Update: [
        "updateSignedBySigner",
        "validUntilInFuture"
      ]
    }
  },

  // -------------------------------------------------------------
  // 37) Oracle consumer guard (freshness/decimals checks)
  // -------------------------------------------------------------
  {
    id: "oracleConsumerGuard",
    label: "Oracle Consumer Guard",
    datumType: "OracleGuardDatum",
    redeemerType: "OracleGuardAction",

    // Library-style guard: no datum needed
    datumFields: [],

    actions: [
      { id: "UseOracle", label: "Use Oracle in Protocol" }
    ],

    constraints: [
      {
        id: "oracleRefPresent",
        label: "Matching oracle reference input present for requested pair"
      },
      {
        id: "oracleFresh",
        label: "Current time < oracle.validUntil"
      },
      {
        id: "decimalsBounded",
        label: "Price decimals / scaling within sanity bounds"
      }
    ],

    defaultActionConstraints: {
      UseOracle: [
        "oracleRefPresent",
        "oracleFresh",
        "decimalsBounded"
      ]
    }
  },

  // -------------------------------------------------------------
  // 38) Governance timelock (queued tx after delay)
  // -------------------------------------------------------------
  {
    id: "governanceTimelock",
    label: "Governance Timelock",
    datumType: "TimelockDatum",
    redeemerType: "TimelockAction",

    // TimelockDatum { txTemplateHash, eta, proposer, timelockNFT }
    datumFields: [
      { id: "txTemplateHash", label: "txTemplateHash :: TokenName",    type: "TokenName" },
      { id: "eta",            label: "eta :: POSIXTime",               type: "POSIXTime" },
      { id: "proposer",       label: "proposer :: PubKeyHash",         type: "PubKeyHash" },
      { id: "timelockNFT",    label: "timelockNFT :: CurrencySymbol",  type: "CurrencySymbol" }
    ],

    actions: [
      { id: "Queue",   label: "Queue Proposal" },
      { id: "Execute", label: "Execute Queued Tx" },
      { id: "Cancel",  label: "Cancel Before ETA" }
    ],

    constraints: [
      {
        id: "executeAfterEta",
        label: "Execute only allowed when now ≥ eta"
      },
      {
        id: "cancelBeforeEta",
        label: "Cancel allowed only when now < eta"
      },
      {
        id: "uniqueTimelockViaNFT",
        label: "Single authoritative timelock enforced by timelockNFT"
      }
    ],

    defaultActionConstraints: {
      Queue: [
        "uniqueTimelockViaNFT"
      ],
      Execute: [
        "uniqueTimelockViaNFT",
        "executeAfterEta"
      ],
      Cancel: [
        "uniqueTimelockViaNFT",
        "cancelBeforeEta"
      ]
    }
  },

  // -------------------------------------------------------------
  // 39) Governor: proposal/vote/execute (token-weighted)
  // -------------------------------------------------------------
  {
    id: "governorVoting",
    label: "Governor (Token-weighted Voting)",
    datumType: "GovernorDatum",
    redeemerType: "GovernorAction",

    // Proposal { actionsHash, snapshotSlot, quorum, forVotes, againstVotes, end }
    datumFields: [
      { id: "actionsHash",  label: "actionsHash :: TokenName", type: "TokenName" },
      { id: "snapshotSlot", label: "snapshotSlot :: Integer",  type: "Integer" },
      { id: "quorum",       label: "quorum :: Integer",        type: "Integer" },
      { id: "forVotes",     label: "forVotes :: Integer",      type: "Integer" },
      { id: "againstVotes", label: "againstVotes :: Integer",  type: "Integer" },
      { id: "end",          label: "end :: POSIXTime",         type: "POSIXTime" }
    ],

    actions: [
      { id: "Propose", label: "Create Proposal" },
      { id: "Vote",    label: "Cast Vote" },
      { id: "Execute", label: "Execute If Passed" }
    ],

    constraints: [
      {
        id: "voteWithinWindow",
        label: "Votes only allowed before end"
      },
      {
        id: "weightMatchesSnapshot",
        label: "Vote weight taken from snapshot at snapshotSlot"
      },
      {
        id: "executeIfPassed",
        label: "Execute only if forVotes ≥ quorum and forVotes > againstVotes"
      },
      {
        id: "singleExecutionGovernor",
        label: "Proposal can be executed only once"
      }
    ],

    defaultActionConstraints: {
      Propose: [],
      Vote: [
        "voteWithinWindow",
        "weightMatchesSnapshot"
      ],
      Execute: [
        "executeIfPassed",
        "singleExecutionGovernor"
      ]
    }
  },

  // -------------------------------------------------------------
  // 40) Snapshot voting (CIP-68 / reference inputs)
  // -------------------------------------------------------------
  {
    id: "snapshotVoting",
    label: "Snapshot Voting (CIP-68)",
    datumType: "SnapshotDatum",
    redeemerType: "SnapshotAction",

    // SnapshotDatum { policyId, slot }
    datumFields: [
      { id: "policyId", label: "policyId :: CurrencySymbol", type: "CurrencySymbol" },
      { id: "slot",     label: "slot :: Integer",            type: "Integer" }
    ],

    actions: [
      { id: "CreateSnapshot", label: "Create Snapshot" },
      { id: "Verify",         label: "Verify Snapshot Use" }
    ],

    constraints: [
      {
        id: "snapshotSlotConsistent",
        label: "Governor uses the same snapshot slot as recorded"
      },
      {
        id: "balancesProofValid",
        label: "Balances are proven via CIP-68 refs/accumulator proofs"
      }
    ],

    defaultActionConstraints: {
      CreateSnapshot: [
        "snapshotSlotConsistent"
      ],
      Verify: [
        "snapshotSlotConsistent",
        "balancesProofValid"
      ]
    }
  },
    // -------------------------------------------------------------
  // 41) Protocol parameters reference UTxO (upgradable config)
  // -------------------------------------------------------------
  {
    id: "protocolParams",
    label: "Protocol Parameters Reference",
    datumType: "ParamsDatum",
    redeemerType: "ParamsAction",

    // Params { version, fees, limits, admins }
    datumFields: [
      { id: "version", label: "version :: Integer", type: "Integer" },
      { id: "fees",    label: "fees :: Integer",    type: "Integer" },
      { id: "limits",  label: "limits :: Integer",  type: "Integer" },
      { id: "admins",  label: "admins :: Integer",  type: "Integer" }
    ],

    actions: [
      { id: "Update", label: "Update Parameters" }
    ],

    constraints: [
      {
        id: "updateGatedByGovernance",
        label: "Updates require governance/timelock authorization"
      },
      {
        id: "versionMonotonic",
        label: "version strictly increases on each update"
      }
    ],

    defaultActionConstraints: {
      Update: ["updateGatedByGovernance", "versionMonotonic"]
    }
  },

  // -------------------------------------------------------------
  // 42) Access-control via NFTs / soulbound badges
  // -------------------------------------------------------------
  {
    id: "accessControlBadges",
    label: "Access Control via SBT Badges",
    datumType: "BadgeDatum",
    redeemerType: "BadgeAction",

    // SBT badges are usually no-datum or simple metadata; keep empty here.
    datumFields: [],

    actions: [
      { id: "MintBadge",   label: "Mint Badge" },
      { id: "RevokeBadge", label: "Revoke Badge" }
    ],

    constraints: [
      {
        id: "onlyControllerCanMintRevoke",
        label: "Mint/Revoke only by controller/governance"
      },
      {
        id: "nonTransferableBadge",
        label: "Badge cannot be transferred (owner must remain the same)"
      }
    ],

    defaultActionConstraints: {
      MintBadge: ["onlyControllerCanMintRevoke"],
      RevokeBadge: ["onlyControllerCanMintRevoke"]
    }
  },

  // -------------------------------------------------------------
  // 43) KYC-gated mint/burn (attestation check)
  // -------------------------------------------------------------
  {
    id: "kycGatedPolicy",
    label: "KYC-gated Mint/Burn Policy",
    datumType: "KycPolicyDatum",
    redeemerType: "KycPolicyAction",

    datumFields: [],

    actions: [
      { id: "Mint", label: "Mint Token" },
      { id: "Burn", label: "Burn Token" }
    ],

    constraints: [
      {
        id: "requiresKycProofRef",
        label: "Requires KYCProof reference input for subjectPKH"
      },
      {
        id: "kycNotExpired",
        label: "KYC proof expiry not passed"
      },
      {
        id: "providerSignatureValid",
        label: "KYC provider signature verified"
      }
    ],

    defaultActionConstraints: {
      Mint: ["requiresKycProofRef", "kycNotExpired", "providerSignatureValid"],
      Burn: ["requiresKycProofRef", "kycNotExpired", "providerSignatureValid"]
    }
  },

  // -------------------------------------------------------------
  // 44) Royalty splitter (creator/protocol payouts)
  // -------------------------------------------------------------
  {
    id: "royaltySplitter",
    label: "Royalty Splitter",
    datumType: "RoyaltySplitterDatum",
    redeemerType: "RoyaltySplitterAction",

    // Splitter { splits[{dest,bps}], splitterNFT, version }
    datumFields: [
      { id: "splitterNFT", label: "splitterNFT :: CurrencySymbol", type: "CurrencySymbol" },
      { id: "version",     label: "version :: Integer",            type: "Integer" }
    ],

    actions: [
      { id: "Route",  label: "Route Royalties" },
      { id: "Update", label: "Update Splits" }
    ],

    constraints: [
      {
        id: "bpsSumTo10000Royalty",
        label: "Configured splits basis points sum to exactly 10,000"
      },
      {
        id: "conservationOfValue",
        label: "Inputs equal sum of outputs (no loss/extra)"
      },
      {
        id: "updateTimelockedAndGoverned",
        label: "Splits update via timelocked governance process"
      }
    ],

    defaultActionConstraints: {
      Route: ["bpsSumTo10000Royalty", "conservationOfValue"],
      Update: ["bpsSumTo10000Royalty", "updateTimelockedAndGoverned"]
    }
  },

  // -------------------------------------------------------------
  // 45) Bridge lockbox / mint-and-burn (custodial sidechain peg)
  // -------------------------------------------------------------
  {
    id: "bridgeLockbox",
    label: "Bridge Lockbox (Mint-and-Burn Peg)",
    datumType: "LockboxDatum",
    redeemerType: "LockboxAction",

    // Lockbox { asset, custodianPKH, lockboxNFT }
    datumFields: [
      { id: "asset",        label: "asset :: CurrencySymbol", type: "CurrencySymbol" },
      { id: "custodianPKH", label: "custodianPKH :: PubKeyHash", type: "PubKeyHash" },
      { id: "lockboxNFT",   label: "lockboxNFT :: CurrencySymbol", type: "CurrencySymbol" }
    ],

    actions: [
      { id: "Lock",   label: "Lock Asset" },
      { id: "Unlock", label: "Unlock Asset" }
    ],

    constraints: [
      {
        id: "custodianSignatureRequired",
        label: "Lock/Unlock requires custodian signature"
      },
      {
        id: "remoteBurnProofRequired",
        label: "Unlock requires proof of remote burn (hash/oracle)"
      },
      {
        id: "lockboxUniquenessViaNFT",
        label: "Unique lockbox instance via lockboxNFT"
      }
    ],

    defaultActionConstraints: {
      Lock: ["custodianSignatureRequired", "lockboxUniquenessViaNFT"],
      Unlock: [
        "custodianSignatureRequired",
        "remoteBurnProofRequired",
        "lockboxUniquenessViaNFT"
      ]
    }
  },

  // -------------------------------------------------------------
  // 46) Flash-loan-style atomic lender (same-tx repayment)
  // -------------------------------------------------------------
  {
    id: "flashLoanLender",
    label: "Flash-loan Lender (Same-tx Repayment)",
    datumType: "FlashLenderDatum",
    redeemerType: "FlashLenderAction",

    // LenderDatum { asset, feeBps, lenderNFT }
    datumFields: [
      { id: "asset",    label: "asset :: CurrencySymbol",   type: "CurrencySymbol" },
      { id: "feeBps",   label: "feeBps :: Integer",         type: "Integer" },
      { id: "lenderNFT",label: "lenderNFT :: CurrencySymbol", type: "CurrencySymbol" }
    ],

    actions: [
      { id: "Borrow", label: "Borrow Flash Loan" }
    ],

    constraints: [
      {
        id: "mustRepayWithFeeInSameTx",
        label: "Transaction must recreate lender UTxO with principal + fee"
      },
      {
        id: "lenderUTxOUnique",
        label: "Lender UTxO enforced unique via lenderNFT"
      }
    ],

    defaultActionConstraints: {
      Borrow: ["mustRepayWithFeeInSameTx", "lenderUTxOUnique"]
    }
  },

  // -------------------------------------------------------------
  // 47) MEV-resistant batcher (deterministic matching rules)
  // -------------------------------------------------------------
  {
    id: "mevResistantBatcher",
    label: "MEV-resistant Batcher",
    datumType: "BatchDatum",
    redeemerType: "BatchAction",

    // Batch { epoch, rulesHash, intentsRoot }
    datumFields: [
      { id: "epoch",      label: "epoch :: Integer",       type: "Integer" },
      { id: "rulesHash",  label: "rulesHash :: TokenName", type: "TokenName" },
      { id: "intentsRoot",label: "intentsRoot :: TokenName", type: "TokenName" }
    ],

    actions: [
      { id: "Settle", label: "Settle Batch" }
    ],

    constraints: [
      {
        id: "deterministicOutcome",
        label: "Settlement recomputes canonical outcome from intentsRoot + rules"
      },
      {
        id: "noExtraValueExtractionBatch",
        label: "Batcher cannot extract additional value vs deterministic result"
      }
    ],

    defaultActionConstraints: {
      Settle: ["deterministicOutcome", "noExtraValueExtractionBatch"]
    }
  },

  // -------------------------------------------------------------
  // 48) NFT floor-price lending (oracle-guarded LTV)
  // -------------------------------------------------------------
  {
    id: "nftFloorLending",
    label: "NFT Floor-price Lending",
    datumType: "NftLoanDatum",
    redeemerType: "NftLoanAction",

    // Loan { nftId, principal, rate, LTV, lastAccrual, borrower }
    datumFields: [
      { id: "nftId",       label: "nftId :: CurrencySymbol", type: "CurrencySymbol" },
      { id: "principal",   label: "principal :: Integer",    type: "Integer" },
      { id: "rate",        label: "rate :: Integer",         type: "Integer" },
      { id: "LTV",         label: "LTV :: Integer",          type: "Integer" },
      { id: "lastAccrual", label: "lastAccrual :: POSIXTime",type: "POSIXTime" },
      { id: "borrower",    label: "borrower :: PubKeyHash",  type: "PubKeyHash" }
    ],

    actions: [
      { id: "Borrow",    label: "Borrow Against NFT" },
      { id: "Repay",     label: "Repay Loan" },
      { id: "Liquidate", label: "Liquidate Loan" }
    ],

    constraints: [
      {
        id: "usesFloorOracleFresh",
        label: "Uses fresh, signed floor-price oracle reference input"
      },
      {
        id: "ltvWithinBounds",
        label: "LTV within allowed range on borrow/repay"
      },
      {
        id: "liquidateIfLtvBreached",
        label: "Liquidation only when LTV above configured threshold"
      }
    ],

    defaultActionConstraints: {
      Borrow: ["usesFloorOracleFresh", "ltvWithinBounds"],
      Repay: ["usesFloorOracleFresh"],
      Liquidate: ["usesFloorOracleFresh", "liquidateIfLtvBreached"]
    }
  },

  // -------------------------------------------------------------
  // 49) Index/basket fund (basket mint with periodic rebalancing)
  // -------------------------------------------------------------
  {
    id: "basketIndexFund",
    label: "Index / Basket Fund",
    datumType: "BasketDatum",
    redeemerType: "BasketAction",

    // Basket { version } – components/weights via separate metadata/ref
    datumFields: [
      { id: "version", label: "version :: Integer", type: "Integer" }
    ],

    actions: [
      { id: "MintBasket",   label: "Mint Basket Token" },
      { id: "RedeemBasket", label: "Redeem Basket Token" },
      { id: "Rebalance",    label: "Rebalance Components" }
    ],

    constraints: [
      {
        id: "depositMatchesTargetWeights",
        label: "Mint requires depositing components in target proportions"
      },
      {
        id: "redeemReturnsProportionalBasket",
        label: "Redeem returns proportional share of basket components"
      },
      {
        id: "rebalanceGoverned",
        label: "Rebalance only by governance/keeper with correct config"
      }
    ],

    defaultActionConstraints: {
      MintBasket: ["depositMatchesTargetWeights"],
      RedeemBasket: ["redeemReturnsProportionalBasket"],
      Rebalance: ["rebalanceGoverned"]
    }
  },

  // -------------------------------------------------------------
  // 50) Insurance mutual (risk pool + claims assessor)
  // -------------------------------------------------------------
  {
    id: "insuranceMutual",
    label: "Insurance Mutual",
    datumType: "MutualPoolDatum",
    redeemerType: "MutualPoolAction",

    // Pool { capital, shares, policyRules }
    datumFields: [
      { id: "capital",    label: "capital :: Integer",    type: "Integer" },
      { id: "shares",     label: "shares :: Integer",     type: "Integer" },
      { id: "policyRules",label: "policyRules :: Integer",type: "Integer" }
    ],

    actions: [
      { id: "Stake",   label: "Stake into Pool" },
      { id: "Unstake", label: "Unstake from Pool" },
      { id: "FileClaim", label: "File Claim" },
      { id: "Approve", label: "Approve Claim" },
      { id: "Reject",  label: "Reject Claim" }
    ],

    constraints: [
      {
        id: "noOverPayout",
        label: "Total claim payouts cannot exceed pool capital"
      },
      {
        id: "claimsNeedQuorumOrTrigger",
        label: "Claim approval requires vote quorum or parametric trigger"
      }
    ],

    defaultActionConstraints: {
      Stake: [],
      Unstake: [],
      FileClaim: [],
      Approve: ["claimsNeedQuorumOrTrigger", "noOverPayout"],
      Reject: ["claimsNeedQuorumOrTrigger"]
    }
  },

  // -------------------------------------------------------------
  // 51) Health — patient consent, claims, access logs
  // -------------------------------------------------------------
  {
    id: "healthConsent",
    label: "Health: Consent & Claims",
    datumType: "ConsentDatum",
    redeemerType: "ConsentAction",

    // Consent { patientPKH, providerPKH, scopes, expiry }
    datumFields: [
      { id: "patient",  label: "patient :: PubKeyHash",  type: "PubKeyHash" },
      { id: "provider", label: "provider :: PubKeyHash", type: "PubKeyHash" },
      { id: "scopes",   label: "scopes :: Integer",      type: "Integer" },
      { id: "expiry",   label: "expiry :: POSIXTime",    type: "POSIXTime" }
    ],

    actions: [
      { id: "Grant",  label: "Grant Consent" },
      { id: "Revoke", label: "Revoke Consent" }
    ],

    constraints: [
      {
        id: "patientSigned",
        label: "Consent grant/revoke signed by patient"
      },
      {
        id: "notExpiredConsent",
        label: "Consent must be unexpired when used"
      }
    ],

    defaultActionConstraints: {
      Grant: ["patientSigned"],
      Revoke: ["patientSigned"]
    }
  },

  // -------------------------------------------------------------
  // 52) Transport & Logistics — shipment + milestones
  // -------------------------------------------------------------
  {
    id: "logisticsShipment",
    label: "Transport & Logistics: Shipment",
    datumType: "ShipmentDatum",
    redeemerType: "ShipmentAction",

    // ShipDatum { custodian, status, deadline }
    datumFields: [
      { id: "custodian", label: "custodian :: PubKeyHash", type: "PubKeyHash" },
      { id: "status",    label: "status :: Integer",       type: "Integer" },
      { id: "deadline",  label: "deadline :: POSIXTime",   type: "POSIXTime" }
    ],

    actions: [
      { id: "AdvanceMilestone", label: "Advance Milestone" },
      { id: "Dispute",          label: "Raise Dispute" }
    ],

    constraints: [
      {
        id: "milestoneProofRequired",
        label: "Advancing milestone requires valid proof hash"
      },
      {
        id: "deadlineRespected",
        label: "Status must resolve before deadline or go to dispute"
      }
    ],

    defaultActionConstraints: {
      AdvanceMilestone: ["milestoneProofRequired", "deadlineRespected"],
      Dispute: ["deadlineRespected"]
    }
  },

  // -------------------------------------------------------------
  // 53) Finance — invoices, factoring, stable rails
  // -------------------------------------------------------------
  {
    id: "financeInvoices",
    label: "Finance: Invoices & Factoring",
    datumType: "InvoiceDatum",
    redeemerType: "InvoiceAction",

    // Invoice { issuer, buyer, amount, due, paid, hash }
    datumFields: [
      { id: "issuer", label: "issuer :: PubKeyHash", type: "PubKeyHash" },
      { id: "buyer",  label: "buyer :: PubKeyHash",  type: "PubKeyHash" },
      { id: "amount", label: "amount :: Integer",    type: "Integer" },
      { id: "due",    label: "due :: POSIXTime",     type: "POSIXTime" },
      { id: "paid",   label: "paid :: Integer",      type: "Integer" },
      { id: "hash",   label: "hash :: TokenName",    type: "TokenName" }
    ],

    actions: [
      { id: "Issue",   label: "Issue Invoice" },
      { id: "Assign",  label: "Assign to Factor" },
      { id: "Pay",     label: "Pay Invoice" }
    ],

    constraints: [
      {
        id: "paymentMatchesInvoice",
        label: "Payment equals outstanding invoice amount"
      }
    ],

    defaultActionConstraints: {
      Issue: [],
      Assign: [],
      Pay: ["paymentMatchesInvoice"]
    }
  },

  // -------------------------------------------------------------
  // 54) Education — certificates + tuition escrow
  // -------------------------------------------------------------
  {
    id: "educationCertificates",
    label: "Education: Certificates & Tuition Escrow",
    datumType: "EducationDatum",
    redeemerType: "EducationAction",

    // Simple combined view
    datumFields: [
      { id: "student", label: "student :: PubKeyHash", type: "PubKeyHash" },
      { id: "school",  label: "school :: PubKeyHash",  type: "PubKeyHash" },
      { id: "amount",  label: "amount :: Integer",     type: "Integer" }
    ],

    actions: [
      { id: "MintCert",   label: "Mint Certificate" },
      { id: "ReleaseFee", label: "Release Tuition" }
    ],

    constraints: [
      {
        id: "schoolSignedForCert",
        label: "Certificate mint requires issuer/school signature"
      }
    ],

    defaultActionConstraints: {
      MintCert: ["schoolSignedForCert"],
      ReleaseFee: []
    }
  },

  // -------------------------------------------------------------
  // 55) Mining — royalties, leases, ore provenance
  // -------------------------------------------------------------
  {
    id: "miningRoyalties",
    label: "Mining: Royalties & Leases",
    datumType: "MiningDatum",
    redeemerType: "MiningAction",

    datumFields: [
      { id: "amount", label: "amount :: Integer", type: "Integer" }
    ],

    actions: [
      { id: "SettleRoyalty", label: "Settle Royalty" },
      { id: "SettleLease",   label: "Settle Lease" }
    ],

    constraints: [
      {
        id: "royaltySplitCorrect",
        label: "Royalty splits follow configured bps"
      }
    ],

    defaultActionConstraints: {
      SettleRoyalty: ["royaltySplitCorrect"],
      SettleLease: []
    }
  },

  // -------------------------------------------------------------
  // 56) Farming & Agriculture — crop parametric + co-op
  // -------------------------------------------------------------
  {
    id: "agriInsuranceCoop",
    label: "Farming: Parametric & Co-op",
    datumType: "AgriDatum",
    redeemerType: "AgriAction",

    // Policy-like summary
    datumFields: [
      { id: "farm",   label: "farm :: PubKeyHash",  type: "PubKeyHash" },
      { id: "sumInsured", label: "sumInsured :: Integer", type: "Integer" },
      { id: "expiry", label: "expiry :: POSIXTime", type: "POSIXTime" }
    ],

    actions: [
      { id: "Payout", label: "Parametric Payout" }
    ],

    constraints: [
      {
        id: "oracleTriggerSatisfied",
        label: "Weather/oracle trigger threshold satisfied"
      }
    ],

    defaultActionConstraints: {
      Payout: ["oracleTriggerSatisfied"]
    }
  },

  // -------------------------------------------------------------
  // 57) Fashion & Beauty — authenticity + royalties
  // -------------------------------------------------------------
  {
    id: "fashionAuthenticity",
    label: "Fashion: Authenticity & Royalties",
    datumType: "FashionDatum",
    redeemerType: "FashionAction",

    datumFields: [
      { id: "brand", label: "brand :: TokenName",    type: "TokenName" },
      { id: "model", label: "model :: TokenName",    type: "TokenName" },
      { id: "serial",label: "serial :: TokenName",   type: "TokenName" }
    ],

    actions: [
      { id: "MintAuthNFT", label: "Mint Authenticity NFT" }
    ],

    constraints: [
      {
        id: "brandSigned",
        label: "Authenticity NFT mint signed by brand"
      }
    ],

    defaultActionConstraints: {
      MintAuthNFT: ["brandSigned"]
    }
  },

  // -------------------------------------------------------------
  // 58) Sports & Entertainment — tickets + revenue split
  // -------------------------------------------------------------
  {
    id: "sportsTickets",
    label: "Sports & Entertainment Tickets",
    datumType: "SportsTicketDatum",
    redeemerType: "SportsTicketAction",

    datumFields: [
      { id: "eventId", label: "eventId :: TokenName", type: "TokenName" },
      { id: "seat",    label: "seat :: TokenName",    type: "TokenName" },
      { id: "rules",   label: "rules :: Integer",     type: "Integer" }
    ],

    actions: [
      { id: "TransferTicket", label: "Transfer Ticket" }
    ],

    constraints: [
      {
        id: "transferRespectsRules",
        label: "Ticket transfer obeys cap/KYC/anti-scalp rules"
      }
    ],

    defaultActionConstraints: {
      TransferTicket: ["transferRespectsRules"]
    }
  },

  // -------------------------------------------------------------
  // 59) Supply Chain — lots + recall tracing
  // -------------------------------------------------------------
  {
    id: "supplyChainLots",
    label: "Supply Chain: Lots & Recall",
    datumType: "LotDatum",
    redeemerType: "LotAction",

    datumFields: [
      { id: "batch",  label: "batch :: TokenName",  type: "TokenName" },
      { id: "qcHash", label: "qcHash :: TokenName", type: "TokenName" }
    ],

    actions: [
      { id: "TransferLot", label: "Transfer Lot" },
      { id: "Recall",      label: "Mark Lot Recalled" }
    ],

    constraints: [
      {
        id: "qcImmutable",
        label: "QC hash immutable across transfers"
      }
    ],

    defaultActionConstraints: {
      TransferLot: ["qcImmutable"],
      Recall: ["qcImmutable"]
    }
  },

  // -------------------------------------------------------------
  // 60) Electronics — warranties + RMA escrow
  // -------------------------------------------------------------
  {
    id: "electronicsWarranty",
    label: "Electronics: Warranty & RMA",
    datumType: "WarrantyDatum",
    redeemerType: "WarrantyAction",

    datumFields: [
      { id: "deviceId", label: "deviceId :: TokenName", type: "TokenName" },
      { id: "expiry",   label: "expiry :: POSIXTime",   type: "POSIXTime" }
    ],

    actions: [
      { id: "ClaimWarranty", label: "Claim Warranty" }
    ],

    constraints: [
      {
        id: "claimBeforeExpiry",
        label: "Warranty claim only before expiry"
      }
    ],

    defaultActionConstraints: {
      ClaimWarranty: ["claimBeforeExpiry"]
    }
  },

  // -------------------------------------------------------------
  // 61) Religion — transparent donations & governance
  // -------------------------------------------------------------
  {
    id: "religionTreasury",
    label: "Religion: Donations & Treasury",
    datumType: "ReligionTreasuryDatum",
    redeemerType: "ReligionTreasuryAction",

    datumFields: [
      { id: "rules", label: "rules :: Integer", type: "Integer" }
    ],

    actions: [
      { id: "Disburse", label: "Disburse Funds" }
    ],

    constraints: [
      {
        id: "disbursementRespectsRules",
        label: "Disbursement matches governance/earmark rules"
      }
    ],

    defaultActionConstraints: {
      Disburse: ["disbursementRespectsRules"]
    }
  },

  // -------------------------------------------------------------
  // 62) Security — bug bounties + incident multisig
  // -------------------------------------------------------------
  {
    id: "securityBounties",
    label: "Security: Bounties & Multisig",
    datumType: "BountyDatum",
    redeemerType: "BountyAction",

    datumFields: [
      { id: "scopeHash", label: "scopeHash :: TokenName", type: "TokenName" },
      { id: "maxPayout", label: "maxPayout :: Integer",   type: "Integer" }
    ],

    actions: [
      { id: "Payout", label: "Pay Bounty" }
    ],

    constraints: [
      {
        id: "arbiterApprovalRequired",
        label: "Bounty payout requires arbiter approval"
      }
    ],

    defaultActionConstraints: {
      Payout: ["arbiterApprovalRequired"]
    }
  },

  // -------------------------------------------------------------
  // 63) Identity — DID/VC + KYC badges
  // -------------------------------------------------------------
  {
    id: "identityVc",
    label: "Identity: DID / VC & KYC Badges",
    datumType: "VcDatum",
    redeemerType: "VcAction",

    // VCRef { issuer, subject, schemaHash, expiry }
    datumFields: [
      { id: "issuer",     label: "issuer :: PubKeyHash",     type: "PubKeyHash" },
      { id: "subject",    label: "subject :: PubKeyHash",    type: "PubKeyHash" },
      { id: "schemaHash", label: "schemaHash :: TokenName",  type: "TokenName" },
      { id: "expiry",     label: "expiry :: POSIXTime",      type: "POSIXTime" }
    ],

    actions: [
      { id: "PublishVC", label: "Publish VC Ref" }
    ],

    constraints: [
      {
        id: "issuerSignedVC",
        label: "VC ref must be signed by issuer"
      }
    ],

    defaultActionConstraints: {
      PublishVC: ["issuerSignedVC"]
    }
  },

  // -------------------------------------------------------------
  // 64) Politics — transparent campaign funds
  // -------------------------------------------------------------
  {
    id: "politicsCampaign",
    label: "Politics: Campaign Treasury",
    datumType: "CampaignTreasuryDatum",
    redeemerType: "CampaignTreasuryAction",

    datumFields: [
      { id: "treasurer",    label: "treasurer :: PubKeyHash",   type: "PubKeyHash" },
      { id: "limits",       label: "limits :: Integer",         type: "Integer" }
    ],

    actions: [
      { id: "Spend", label: "Spend from Treasury" }
    ],

    constraints: [
      {
        id: "spendWithinLimits",
        label: "Spending respects configured campaign limits"
      }
    ],

    defaultActionConstraints: {
      Spend: ["spendWithinLimits"]
    }
  },

  // -------------------------------------------------------------
  // 65) News — provenance + pay-per-article
  // -------------------------------------------------------------
  {
    id: "newsProvenancePaywall",
    label: "News: Provenance & Pay-per-Article",
    datumType: "NewsDatum",
    redeemerType: "NewsAction",

    // ArticleRef { publisherPKH, contentHash, ts }
    datumFields: [
      { id: "publisher",   label: "publisher :: PubKeyHash", type: "PubKeyHash" },
      { id: "contentHash", label: "contentHash :: TokenName",type: "TokenName" },
      { id: "ts",          label: "ts :: POSIXTime",         type: "POSIXTime" }
    ],

    actions: [
      { id: "GrantAccess", label: "Grant Article Access" }
    ],

    constraints: [
      {
        id: "paymentMatchesPrice",
        label: "Reader payment covers article price"
      }
    ],

    defaultActionConstraints: {
      GrantAccess: ["paymentMatchesPrice"]
    }
  },

  // -------------------------------------------------------------
  // 66) Social Media — creator payouts + token-gating
  // -------------------------------------------------------------
  {
    id: "socialCreator",
    label: "Social Media: Creator Payouts & Token-gating",
    datumType: "SocialDatum",
    redeemerType: "SocialAction",

    datumFields: [
      { id: "author", label: "author :: PubKeyHash", type: "PubKeyHash" }
    ],

    actions: [
      { id: "PayCreator", label: "Pay Creator" }
    ],

    constraints: [
      {
        id: "splitToAuthorAndTreasury",
        label: "Payment split between author and protocol treasury"
      }
    ],

    defaultActionConstraints: {
      PayCreator: ["splitToAuthorAndTreasury"]
    }
  },

  // -------------------------------------------------------------
  // 67) Construction — milestone escrow & liens
  // -------------------------------------------------------------
  {
    id: "constructionMilestones",
    label: "Construction: Milestone Escrow & Liens",
    datumType: "ConstructionDatum",
    redeemerType: "ConstructionAction",

    datumFields: [
      { id: "owner", label: "owner :: PubKeyHash", type: "PubKeyHash" },
      { id: "step",  label: "step :: Integer",     type: "Integer" }
    ],

    actions: [
      { id: "UnlockStep", label: "Unlock Step" }
    ],

    constraints: [
      {
        id: "inspectorApprovalRequired",
        label: "Step unlock requires inspector/attestor approval"
      }
    ],

    defaultActionConstraints: {
      UnlockStep: ["inspectorApprovalRequired"]
    }
  },

  // -------------------------------------------------------------
  // 68) Communication — prepaid vouchers & spam deposits
  // -------------------------------------------------------------
  {
    id: "communicationVouchers",
    label: "Communication: Vouchers & Spam Deposits",
    datumType: "CommDatum",
    redeemerType: "CommAction",

    datumFields: [
      { id: "quota",  label: "quota :: Integer",  type: "Integer" },
      { id: "expiry", label: "expiry :: POSIXTime", type: "POSIXTime" }
    ],

    actions: [
      { id: "RedeemVoucher", label: "Redeem Voucher" }
    ],

    constraints: [
      {
        id: "redeemBeforeExpiry",
        label: "Voucher redemption only before expiry"
      }
    ],

    defaultActionConstraints: {
      RedeemVoucher: ["redeemBeforeExpiry"]
    }
  },

  // -------------------------------------------------------------
  // 69) Catering — event deposits + supplier splits
  // -------------------------------------------------------------
  {
    id: "cateringEventEscrow",
    label: "Catering: Event Escrow & Splits",
    datumType: "CateringDatum",
    redeemerType: "CateringAction",

    datumFields: [
      { id: "client",    label: "client :: PubKeyHash",   type: "PubKeyHash" },
      { id: "caterer",   label: "caterer :: PubKeyHash",  type: "PubKeyHash" },
      { id: "headcount", label: "headcount :: Integer",   type: "Integer" }
    ],

    actions: [
      { id: "SettleEvent", label: "Settle Event" }
    ],

    constraints: [
      {
        id: "settlePerHeadRules",
        label: "Settlement depends on final headcount and price rules"
      }
    ],

    defaultActionConstraints: {
      SettleEvent: ["settlePerHeadRules"]
    }
  },

  // -------------------------------------------------------------
  // 70) Energy — P2P energy + carbon credits
  // -------------------------------------------------------------
  {
    id: "energyP2P",
    label: "Energy: P2P & Carbon Credits",
    datumType: "EnergyDatum",
    redeemerType: "EnergyAction",

    datumFields: [
      { id: "kwh",   label: "kwh :: Integer",   type: "Integer" },
      { id: "carbon",label: "carbon :: Integer",type: "Integer" }
    ],

    actions: [
      { id: "RetireCarbon", label: "Retire Carbon Credit" }
    ],

    constraints: [
      {
        id: "retireBurnsCarbon",
        label: "Retiring carbon burns/locks credit token"
      }
    ],

    defaultActionConstraints: {
      RetireCarbon: ["retireBurnsCarbon"]
    }
  },

  // -------------------------------------------------------------
  // 71) Events — NFT tickets + VIP access
  // -------------------------------------------------------------
  {
    id: "eventsTicketsVip",
    label: "Events: Tickets & VIP Access",
    datumType: "EventTicketDatum",
    redeemerType: "EventTicketAction",

    datumFields: [
      { id: "tier",  label: "tier :: Integer",    type: "Integer" },
      { id: "eventId", label: "eventId :: TokenName", type: "TokenName" }
    ],

    actions: [
      { id: "UseTicket", label: "Use Ticket at Gate" }
    ],

    constraints: [
      {
        id: "ticketValidForEvent",
        label: "Ticket valid only for matching event and tier rules"
      }
    ],

    defaultActionConstraints: {
      UseTicket: ["ticketValidForEvent"]
    }
  },

  // -------------------------------------------------------------
  // 72) Tourism — bookings + loyalty passes
  // -------------------------------------------------------------
  {
    id: "tourismBookings",
    label: "Tourism: Bookings & Loyalty",
    datumType: "BookingDatum",
    redeemerType: "BookingAction",

    datumFields: [
      { id: "host",   label: "host :: PubKeyHash",   type: "PubKeyHash" },
      { id: "guest",  label: "guest :: PubKeyHash",  type: "PubKeyHash" },
      { id: "price",  label: "price :: Integer",     type: "Integer" }
    ],

    actions: [
      { id: "CheckIn",  label: "Check-in / Confirm Stay" },
      { id: "Cancel",   label: "Cancel Booking" }
    ],

    constraints: [
      {
        id: "cancelPolicyRespected",
        label: "Refund/cancel obeys booking cancel policy"
      }
    ],

    defaultActionConstraints: {
      CheckIn: [],
      Cancel: ["cancelPolicyRespected"]
    }
  },

  // -------------------------------------------------------------
  // 73) Insurance — parametric & mutual
  // -------------------------------------------------------------
  {
    id: "insuranceParametricMutual",
    label: "Insurance: Parametric & Mutual",
    datumType: "InsuranceDatum",
    redeemerType: "InsuranceAction",

    datumFields: [
      { id: "sumInsured", label: "sumInsured :: Integer", type: "Integer" },
      { id: "expiry",     label: "expiry :: POSIXTime",   type: "POSIXTime" }
    ],

    actions: [
      { id: "ParametricPayout", label: "Parametric Payout" },
      { id: "VoteClaim",        label: "Vote on Claim" }
    ],

    constraints: [
      {
        id: "parametricTriggerSatisfied2",
        label: "Parametric payout only when trigger satisfied"
      }
    ],

    defaultActionConstraints: {
      ParametricPayout: ["parametricTriggerSatisfied2"],
      VoteClaim: []
    }
  },

  // -------------------------------------------------------------
  // 74) Jobs — payroll streams + bounties
  // -------------------------------------------------------------
  {
    id: "jobsPayrollBounties",
    label: "Jobs: Payroll Streams & Bounties",
    datumType: "JobsDatum",
    redeemerType: "JobsAction",

    datumFields: [
      { id: "rate",  label: "rate :: Integer",  type: "Integer" },
      { id: "start", label: "start :: POSIXTime", type: "POSIXTime" },
      { id: "end",   label: "end :: POSIXTime",   type: "POSIXTime" }
    ],

    actions: [
      { id: "ClaimStream", label: "Claim Payroll" }
    ],

    constraints: [
      {
        id: "claimWithinScheduledPeriod",
        label: "Payroll claims only within [start, end]"
      }
    ],

    defaultActionConstraints: {
      ClaimStream: ["claimWithinScheduledPeriod"]
    }
  },

  // -------------------------------------------------------------
  // 75) NFTs — rental, fractional, royalties
  // -------------------------------------------------------------
  {
    id: "nftsAdvanced",
    label: "NFTs: Rental, Fractional, Royalties",
    datumType: "NftAdvancedDatum",
    redeemerType: "NftAdvancedAction",

    datumFields: [
      { id: "nft",   label: "nft :: CurrencySymbol", type: "CurrencySymbol" },
      { id: "shares",label: "shares :: Integer",     type: "Integer" }
    ],

    actions: [
      { id: "Lease",  label: "Lease NFT" },
      { id: "Redeem", label: "Redeem NFT from Vault" }
    ],

    constraints: [
      {
        id: "leaseRespectsExpiry",
        label: "Lease use-rights expire at lease expiry"
      }
    ],

    defaultActionConstraints: {
      Lease: ["leaseRespectsExpiry"],
      Redeem: []
    }
  },

  // -------------------------------------------------------------
  // 76) Tokens — curves, vesting, compliance
  // -------------------------------------------------------------
  {
    id: "tokensCurvesVestingCompliance",
    label: "Tokens: Curves, Vesting, Compliance",
    datumType: "TokenPolicyDatum",
    redeemerType: "TokenPolicyAction",

    datumFields: [],

    actions: [
      { id: "MintCurve",  label: "Mint via Curve" },
      { id: "ClaimVested",label: "Claim Vested Tokens" },
      { id: "Transfer",   label: "Transfer with Compliance" }
    ],

    constraints: [
      {
        id: "complianceRefChecked",
        label: "Transfer checks ComplianceRef"
      }
    ],

    defaultActionConstraints: {
      MintCurve: [],
      ClaimVested: [],
      Transfer: ["complianceRefChecked"]
    }
  },

  // -------------------------------------------------------------
  // 77) AI — compute escrows + data-label bounties
  // -------------------------------------------------------------
  {
    id: "aiComputeJobs",
    label: "AI: Compute Escrows & Label Bounties",
    datumType: "AiJobDatum",
    redeemerType: "AiJobAction",

    datumFields: [
      { id: "specHash", label: "specHash :: TokenName", type: "TokenName" },
      { id: "reward",   label: "reward :: Integer",     type: "Integer" },
      { id: "deadline", label: "deadline :: POSIXTime", type: "POSIXTime" }
    ],

    actions: [
      { id: "PayoutJob", label: "Payout for Job" }
    ],

    constraints: [
      {
        id: "verifierApprovalRequired",
        label: "Job payout requires verifier approval of result hash"
      }
    ],

    defaultActionConstraints: {
      PayoutJob: ["verifierApprovalRequired"]
    }
  },

  // -------------------------------------------------------------
  // 78) Military — procurement trails + badges
  // -------------------------------------------------------------
  {
    id: "militaryProcurement",
    label: "Military: Procurement & Badges",
    datumType: "MilitaryDatum",
    redeemerType: "MilitaryAction",

    datumFields: [
      { id: "nsn",   label: "nsn :: TokenName",   type: "TokenName" },
      { id: "unit",  label: "unit :: TokenName",  type: "TokenName" }
    ],

    actions: [
      { id: "RecordProcurement", label: "Record Procurement" }
    ],

    constraints: [
      {
        id: "roleBadgeRequired",
        label: "Only addresses with proper RoleSBT can record procurement"
      }
    ],

    defaultActionConstraints: {
      RecordProcurement: ["roleBadgeRequired"]
    }
  },

  // -------------------------------------------------------------
  // 79) Customers (Rewards) — points + tiers
  // -------------------------------------------------------------
  {
    id: "customerRewards",
    label: "Customer Rewards: Points & Tiers",
    datumType: "RewardsDatum",
    redeemerType: "RewardsAction",

    datumFields: [
      { id: "balance", label: "balance :: Integer", type: "Integer" }
    ],

    actions: [
      { id: "AccruePoints", label: "Accrue Points" },
      { id: "RedeemPoints", label: "Redeem Points" }
    ],

    constraints: [
      {
        id: "noOverRedemption",
        label: "Cannot redeem more points than balance"
      }
    ],

    defaultActionConstraints: {
      AccruePoints: [],
      RedeemPoints: ["noOverRedemption"]
    }
  },

  // -------------------------------------------------------------
  // 80) Membership — SBTs + DAO weight
  // -------------------------------------------------------------
  {
    id: "membershipDaoWeight",
    label: "Membership: SBTs & DAO Weight",
    datumType: "DaoMemberDatum",
    redeemerType: "DaoMemberAction",

    datumFields: [
      { id: "org",    label: "org :: CurrencySymbol", type: "CurrencySymbol" },
      { id: "joined", label: "joined :: POSIXTime",   type: "POSIXTime" },
      { id: "expiry", label: "expiry :: POSIXTime",   type: "POSIXTime" }
    ],

    actions: [
      { id: "JoinOrg",   label: "Join Org" },
      { id: "RenewOrg",  label: "Renew Membership" }
    ],

    constraints: [
      {
        id: "nonTransferableMemberSbt",
        label: "Membership token is non-transferable"
      }
    ],

    defaultActionConstraints: {
      JoinOrg: ["nonTransferableMemberSbt"],
      RenewOrg: ["nonTransferableMemberSbt"]
    }
  },

  // -------------------------------------------------------------
  // 81) Access Control — token-gated doors/APIs
  // -------------------------------------------------------------
  {
    id: "accessControlPass",
    label: "Access Control: Doors & APIs",
    datumType: "AccessPassDatum",
    redeemerType: "AccessPassAction",

    datumFields: [
      { id: "resourceId", label: "resourceId :: TokenName", type: "TokenName" },
      { id: "expiry",     label: "expiry :: POSIXTime",     type: "POSIXTime" }
    ],

    actions: [
      { id: "UsePass", label: "Use Access Pass" }
    ],

    constraints: [
      {
        id: "passNotExpired",
        label: "Access only if pass not expired and not revoked"
      }
    ],

    defaultActionConstraints: {
      UsePass: ["passNotExpired"]
    }
  },

  // -------------------------------------------------------------
  // 82) Aviation — maintenance + fees
  // -------------------------------------------------------------
  {
    id: "aviationMaintenance",
    label: "Aviation: Maintenance & Fees",
    datumType: "AviationDatum",
    redeemerType: "AviationAction",

    datumFields: [
      { id: "tailNo", label: "tailNo :: TokenName",  type: "TokenName" },
      { id: "fee",    label: "fee :: Integer",       type: "Integer" }
    ],

    actions: [
      { id: "RecordMaintenance", label: "Record Maintenance" },
      { id: "PaySlotFee",        label: "Pay Slot/Landing Fee" }
    ],

    constraints: [
      {
        id: "certifierSignedMaintenance",
        label: "Maintenance records must be signed by certifier"
      }
    ],

    defaultActionConstraints: {
      RecordMaintenance: ["certifierSignedMaintenance"],
      PaySlotFee: []
    }
  },

  // -------------------------------------------------------------
  // 83) Cross-Border — remittance + compliance
  // -------------------------------------------------------------
  {
    id: "crossBorderRemittance",
    label: "Cross-Border Remittance & Compliance",
    datumType: "RemitDatum",
    redeemerType: "RemitAction",

    // Remit { sender, receiver, amount, corridor }
    datumFields: [
      { id: "sender",   label: "sender :: PubKeyHash",  type: "PubKeyHash" },
      { id: "receiver", label: "receiver :: PubKeyHash",type: "PubKeyHash" },
      { id: "amount",   label: "amount :: Integer",     type: "Integer" },
      { id: "corridor", label: "corridor :: TokenName", type: "TokenName" }
    ],

    actions: [
      { id: "SendRemit", label: "Send Remittance" }
    ],

    constraints: [
      {
        id: "kycAndFxOracleChecked",
        label: "KYCRef and FX oracle ref checked for the corridor"
      }
    ],

    defaultActionConstraints: {
      SendRemit: ["kycAndFxOracleChecked"]
    }
  }


];
