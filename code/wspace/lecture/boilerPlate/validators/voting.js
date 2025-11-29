export const votingFile = `{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Voting where

import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import Prelude (IO)

------------------------------------------------------------
-- PARAMETERS
------------------------------------------------------------

data VotingParams = VotingParams
  { vpAdmin :: PubKeyHash
  }
PlutusTx.unstableMakeIsData ''VotingParams
PlutusTx.makeLift        ''VotingParams

------------------------------------------------------------
-- DATUM & REDEEMER
------------------------------------------------------------

-- | A simple on-chain tally: (candidate, votes)
type Candidate = BuiltinByteString

data VoteDatum = VoteDatum
  { vdOpen     :: Bool
  , vdVoters   :: [PubKeyHash]                 -- voters who already voted
  , vdTallies  :: [(Candidate, Integer)]       -- per-candidate vote counts
  }
PlutusTx.unstableMakeIsData ''VoteDatum

data VoteAction
  = Vote Candidate
  | Close
PlutusTx.unstableMakeIsData ''VoteAction

------------------------------------------------------------
-- UTILITIES
------------------------------------------------------------

{-# INLINABLE getSingleSigner #-}
getSingleSigner :: TxInfo -> Maybe PubKeyHash
getSingleSigner info =
  case txInfoSignatories info of
    [pkh] -> Just pkh
    _     -> Nothing

{-# INLINABLE hasVoted #-}
hasVoted :: PubKeyHash -> [PubKeyHash] -> Bool
hasVoted p = any (\q -> q == p)

{-# INLINABLE incTally #-}
incTally :: Candidate -> [(Candidate, Integer)] -> [(Candidate, Integer)]
incTally c [] = [(c, 1)]
incTally c ((c', n):xs)
  | c == c'   = (c, n + 1) : xs
  | otherwise = (c', n)    : incTally c xs

{-# INLINABLE valuePreserved #-}
-- Require that the value locked at the script stays the same during a vote
valuePreserved :: TxOut -> TxOut -> Bool
valuePreserved inp out = txOutValue inp == txOutValue out

{-# INLINABLE ownInput #-}
ownInput :: ScriptContext -> TxOut
ownInput ctx =
  case findOwnInput ctx of
    Just i  -> txInInfoResolved i
    Nothing -> traceError "own input not found"

{-# INLINABLE continuingOutputs #-}
continuingOutputs :: ScriptContext -> [TxOut]
continuingOutputs = getContinuingOutputs

{-# INLINABLE outputDatumInline #-}
-- Extract the inline datum from a TxOut (V2)
outputDatumInline :: TxOut -> Datum
outputDatumInline o =
  case txOutDatum o of
    OutputDatum d       -> d
    OutputDatumHash _   -> traceError "expected inline datum"
    NoOutputDatum       -> traceError "no datum"
    -- Some ledgers expose OutputDatumInline; OutputDatum is inline in V2 API.

{-# INLINABLE decodeDatum #-}
decodeDatum :: Datum -> VoteDatum
decodeDatum (Datum d) = unsafeFromBuiltinData d

------------------------------------------------------------
-- VALIDATOR
------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: VotingParams -> VoteDatum -> VoteAction -> ScriptContext -> Bool
mkValidator params datum redeemer ctx =
  case redeemer of
    Vote candidate ->
         traceIfFalse "voting closed"          (vdOpen datum)
      && traceIfFalse "need single signer"     (isJust signerM)
      && traceIfFalse "already voted"          (maybe False (\s -> not (hasVoted s (vdVoters datum))) signerM)
      && traceIfFalse "exactly one cont. out"  (length contOuts == 1)
      && traceIfFalse "value changed"          (valuePreserved inRef (head contOuts))
      && traceIfFalse "bad updated datum"      (checkUpdatedDatum datum (head contOuts) signerM candidate)

    Close ->
         traceIfFalse "not admin"              (txSignedBy info (vpAdmin params))
      && traceIfFalse "must consume script"    True
      && traceIfFalse "no continuing outputs"  (null contOuts)
  where
    info      :: TxInfo
    info      = scriptContextTxInfo ctx
    signerM   :: Maybe PubKeyHash
    signerM   = getSingleSigner info

    inRef     :: TxOut
    inRef     = ownInput ctx

    contOuts  :: [TxOut]
    contOuts  = continuingOutputs ctx

    -- Verify the continuing output carries the correct updated datum
    {-# INLINABLE checkUpdatedDatum #-}
    checkUpdatedDatum :: VoteDatum -> TxOut -> Maybe PubKeyHash -> Candidate -> Bool
    checkUpdatedDatum d out mSigner cand =
      let d' = decodeDatum (outputDatumInline out)
      in case mSigner of
          Nothing   -> False
          Just s    ->
               vdOpen d'                                      -- still open after a vote
            && vdVoters d' == (s : vdVoters d)                -- prepend current voter
            && vdTallies d' == incTally cand (vdTallies d)    -- increment candidate

------------------------------------------------------------
-- UNTYPED WRAPPER & COMPILED VALIDATOR
------------------------------------------------------------

{-# INLINABLE mkUntyped #-}
mkUntyped
  :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntyped p d r c =
  let params = unsafeFromBuiltinData p
      datum  = unsafeFromBuiltinData d
      red    = unsafeFromBuiltinData r
      ctx    = unsafeFromBuiltinData c
  in  check (mkValidator params datum red ctx)
  where
    check True  = ()
    check False = traceError "validation failed"

validator :: VotingParams -> Validator
validator params =
  mkValidatorScript $
    $$(PlutusTx.compile [|| \ps -> mkUntyped ps ||])
      \`PlutusTx.applyCode\` PlutusTx.liftCode params

validatorHash' :: VotingParams -> ValidatorHash
validatorHash' = validatorHash . validator

-- If you need an address (V2): Address (ScriptCredential vh) Nothing
validatorAddress :: VotingParams -> Address
validatorAddress ps = Address (ScriptCredential (validatorHash' ps)) Nothing
`