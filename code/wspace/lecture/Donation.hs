module Donation(DonationDatum(..)) where

data DonationDatum = DonationDatum
  {
    deadLineReached :: Bool
  } deriving (Show, Eq)