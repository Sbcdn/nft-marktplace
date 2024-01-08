{-
  Author   : Torben Poguntke
  Copyright: 2023
  Version  : v2.0
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}

import           Cardano.Api
import           Cardano.Api.Shelley          (Address (ShelleyAddress))
import           Cardano.Ledger.Alonzo.TxInfo (transKeyHash)
import           Cardano.Ledger.Shelley.API   (Credential (KeyHashObj))
import           Data.Text
import           Ledger
import           Plutus.Contract.CardanoAPI   (toCardanoAddressInEra)
import           Prelude
import           System.Environment           (getArgs)

import Types
import OffChain

main :: IO ()
main = do
    args <- getArgs
    case args of
      
      [pkh',magic'] -> do
        let
            scriptFile   = "scripts/nft_marketplace.plutus"
            pkh          = addrToPkh $ either (\_ -> error "Not a valid address") id (parseShelleyAddr pkh')
            params          = ScriptParams
                {
                  pFee         = 25      -- 2.5 percent fee of selling price
                , pAddr        = pkh
                }
            magic = case read magic' :: Integer of        -- 1..n: (Testnet (NetworkMagic n)) || 0: Mainnet
              0 -> Mainnet
              x -> Testnet (NetworkMagic $ fromInteger x)
            address     = scriptAddress params
            address'    = case toCardanoAddressInEra magic address of
                          Left err    -> error $ "cannot create bech32 contract address: " ++ show err
                          Right addr' -> serialiseAddress addr'
        scriptResult <- writeFileTextEnvelope scriptFile Nothing $ apiScript params
        case scriptResult of
            Left err -> print $ displayError err
            Right () -> Prelude.putStrLn $ "{\"plutus_file\":" ++ show scriptFile ++ ",\"script_address\":" ++ show address' ++ ",\"owner\":" ++ show pkh' ++"}"
      _ -> error "You need to provide smart contract owner address and network magic."
-- read and decode bech32 Cardano address
parseShelleyAddr :: String -> Either Bech32DecodeError (Cardano.Api.Shelley.Address ShelleyAddr)
parseShelleyAddr s = deserialiseFromBech32 AsShelleyAddress $ Data.Text.pack s

-- get PKH from bech32 address
addrToPkh :: Cardano.Api.Shelley.Address ShelleyAddr -> PubKeyHash
addrToPkh (ShelleyAddress _ (KeyHashObj kh) _) = transKeyHash kh
addrToPkh _                                    = error "addrToPkh"

