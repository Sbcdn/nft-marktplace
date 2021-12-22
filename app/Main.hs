{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


import Cardano.Api                         hiding (TxId)
import Data.String                         (IsString (..))
import Ledger
import Ledger.Bytes                        (getLedgerBytes)
import Prelude
import System.Environment                  (getArgs)

import qualified Data.ByteString.Lazy as B

import Data.Text
import PlutusTx.Builtins

import Types
import OffChain

main :: IO ()
main = do
    [pkh', pak']      <- getArgs
    let 
        scriptFile   = "scripts/nami-script.plutus"
        pkhs            = parsePkh pkh'
        ppak            = parsePkh pak'
        params          = ScriptParams
            {
              pFee         = 25      -- 2.5 percent fee of selling price 
            , pAK          = ppak    
            , pAddr        = pkhs
            }

    scriptResult <- writeFileTextEnvelope scriptFile Nothing $ apiScript params
    case scriptResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote NFT policy to file " ++ show scriptFile

parsePkh :: String -> PubKeyHash
parsePkh s = PubKeyHash $ getLedgerBytes $ fromString s

    -- w1: 344b486fd05957b8c7814e9aef3056d18a63dfcff0563d1ef0106b4d
    -- wMain: 64b8ca3800ae326e190492aafda22e0a5836d75b51d4de76ff2d7c4a
    -- get PubKey or PubKeyHash from .vkey file
    -- cabal repl
    -- import Cardano.Api Ledger PlutusTx.Builtins
    -- Right vkey <- readFileTextEnvelope (AsVerificationKey AsPaymentKey) "/home/tp/Downloads/alonzo-purpule/cardano-node-1.30.1-linux/addr/testnet/wMain/main_testnet.skey"
    -- To PubKey Hash: PubKeyHash $ toBuiltin $ serialiseToRawBytes $ verificationKeyHash vkey
    -- To PubKey: PubKey $ Ledger.Bytes.fromBytes $ serialiseToRawBytes vkey
