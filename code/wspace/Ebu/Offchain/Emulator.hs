{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

-- base
import Control.Monad (void)

-- ledger / plutus
import Ledger
import Ledger.Ada as Ada
import Plutus.Contract
import Plutus.Trace.Emulator as Emulator
import Wallet.Emulator.Wallet

-- reuse YOUR on-chain code
import OnChain.PublicFund

------------------------------------------------------------
-- Schema
------------------------------------------------------------

type EscrowSchema =
        Endpoint "lock" ()
    .\/ Endpoint "approve" ()
    .\/ Endpoint "release" ()
    .\/ Endpoint "refund" ()

------------------------------------------------------------
-- Script address
------------------------------------------------------------

escrowAddress :: Address
escrowAddress = scriptAddress validator

------------------------------------------------------------
-- Datum
------------------------------------------------------------

mkDatum :: Wallet -> Wallet -> [Wallet] -> POSIXTime -> EscrowDatum
mkDatum depositor beneficiary officials deadline =
    EscrowDatum
        { edDepositor   = mockWalletPaymentPubKeyHash depositor
        , edBeneficiary = mockWalletPaymentPubKeyHash beneficiary
        , edOfficials   = fmap mockWalletPaymentPubKeyHash officials
        , edApprovals   = []
        , edRequired    = 2
        , edDeadline    = deadline
        }

------------------------------------------------------------
-- Endpoints
------------------------------------------------------------

lock :: Contract () EscrowSchema Text ()
lock = do
    let datum =
            mkDatum (knownWallet 1) (knownWallet 2)
                    [knownWallet 3, knownWallet 4]
                    20_000

        tx =
            mustPayToTheScript datum
                (Ada.lovelaceValueOf 10_000_000)

    void $ submitTxConstraints validator tx

approve :: Contract () EscrowSchema Text ()
approve = do
    utxos <- utxosAt escrowAddress
    case utxos of
        [(oref, _)] ->
            void $
              submitTxConstraintsSpending
                validator
                utxos
                (mustSpendScriptOutput oref
                    (Redeemer $ toBuiltinData Approve))
        _ -> logError @String "No script UTxO"

release :: Contract () EscrowSchema Text ()
release = do
    utxos <- utxosAt escrowAddress
    case utxos of
        [(oref, _)] ->
            void $
              submitTxConstraintsSpending
                validator
                utxos
                (mustSpendScriptOutput oref
                    (Redeemer $ toBuiltinData Release))
        _ -> logError @String "No script UTxO"

------------------------------------------------------------
-- Emulator trace
------------------------------------------------------------

trace :: EmulatorTrace ()
trace = do
    h1 <- activateContractWallet (knownWallet 1) lock
    void $ Emulator.waitNSlots 1

    h3 <- activateContractWallet (knownWallet 3) approve
    void $ Emulator.waitNSlots 1

    h4 <- activateContractWallet (knownWallet 4) approve
    void $ Emulator.waitNSlots 1

    h2 <- activateContractWallet (knownWallet 2) release
    void $ Emulator.waitNSlots 1

------------------------------------------------------------
-- Main
------------------------------------------------------------

main :: IO ()
main =
    runEmulatorTraceIO trace
