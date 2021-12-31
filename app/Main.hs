{-# LANGUAGE OverloadedStrings   #-}


import Cardano.Api                         hiding (TxId)
import Data.String                         (IsString (..))
import Ledger
import Ledger.Bytes                        (getLedgerBytes)
import Prelude
import System.Environment                  (getArgs)

import Types
import OffChain

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> error "You need to provide PubKeyHash!"
      (pkh':_) -> do
        let
            scriptFile   = "scripts/artifct-lbuc.plutus"
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

parsePkh :: String -> PubKeyHash
parsePkh s = PubKeyHash $ getLedgerBytes $ fromString s

