module Main where

import Utilities

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    writeValidator "./assets/validator.plutus" validator
    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator
    putStrLn $ "--- Escrow Info ---"
    --putStrLn $ "Validator Hash (Plutus): " <> show vh
    --putStrLn $ "Plutus Script Address:    " <> show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "Escrow NFT validator generated successfully."


