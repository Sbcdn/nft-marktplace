{-# LANGUAGE OverloadedStrings   #-}


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
    [pkh']      <- getArgs
    let 
        scriptFile   = "scripts/artifct-lubc.plutus"
        pkhs            = parsePkh pkh'
        params          = ScriptParams
            {
              pFee         = 25      -- 2.5 percent fee of selling price   
            , pAddr        = pkhs
            }

    scriptResult <- writeFileTextEnvelope scriptFile Nothing $ apiScript params
    case scriptResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote NFT policy to file " ++ show scriptFile

parsePk :: String -> PubKey
parsePk s = PubKey $ getPubKey $ fromString s

parsePkh :: String -> PubKeyHash
parsePkh s = PubKeyHash $ getLedgerBytes $ fromString s

