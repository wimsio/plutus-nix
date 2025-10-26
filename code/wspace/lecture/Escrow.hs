
module Escrow(EscrowDatum(..), EscrowRedeemer(..)) where

import Plutus.V2.Ledger.Api (PubKeyHash)

data EscrowDatum = EscrowDatum 
  {
    creator :: PubKeyHash    
  } deriving(Show,Eq)

data EscrowRedeemer = Withdraw | Cancel deriving(Show,Eq )




