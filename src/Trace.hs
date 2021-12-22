{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

{-| Example trace for the uniswap contract
-}
module Trace(
    uniswapTrace
    --
    , setupTokens
    , tokenNames
    , wallets
    , test
    ) where

import           Data.Aeson                     (FromJSON (..), ToJSON (..), ToJSONKey)
import           Data.Hashable                  (Hashable (..))
import           GHC.Generics                   (Generic (..))

import           Control.Monad                     (forM_, when)
import qualified Plutus.Trace.Emulator             as Trace
import qualified Data.Map                          as Map
import qualified Data.Monoid                       as Monoid
import qualified Data.Semigroup                    as Semigroup
import           Data.Text                         (Text(..),pack)
import           Ledger
import           Ledger.Ada                        (adaSymbol, adaToken)
import           Ledger.Constraints
import           Ledger.Value                      as Value
import           PlutusTx.Prelude                  (sha2_256)
import           Plutus.Contract
import qualified Plutus.Contracts.Currency         as Currency
import           OffChain                          as OffChain
import           Types                             as Types
import           Plutus.Trace.Emulator             (EmulatorRuntimeError (GenericError), EmulatorTrace)
import qualified Plutus.Trace.Emulator             as Emulator
import           Wallet.Emulator
import           Wallet.Emulator.Wallet
import           Wallet.Emulator.Types
import qualified Ledger.Crypto                  as Crypto
import qualified Wallet.API                     as WAPI
import           Wallet.Effects                 (NodeClientEffect, WalletEffect (..), publishTx)
import           Wallet.Emulator.Chain          (ChainState (..))
import           Wallet.Emulator.LogMessages    (RequestHandlerLogMsg, TxBalanceMsg (..))
import           Wallet.Emulator.NodeClient     (NodeClientState, emptyNodeClientState)

import           PlutusTx.Builtins.Internal
import           Plutus.V1.Ledger.Bytes
import           PlutusTx.Builtins.Class

test :: IO ()
test = Emulator.runEmulatorTraceIO uniswapTrace

-- | Set up a liquidity pool and call the "add" endpoint
uniswapTrace :: EmulatorTrace ()
uniswapTrace = do
    cidInit <- Emulator.activateContract (knownWallet 1) setupTokens "init"
    _ <- Emulator.waitNSlots 5
    cs <- Emulator.observableState cidInit >>= \case
                Just (Semigroup.Last cur) -> pure (Currency.currencySymbol cur)
                _                         -> Trace.throwError $ GenericError "failed to create currency"
    let tokens = Map.fromList [(tn, Types.mkToken cs tn) | tn <- tokenNames]
        ada   = Types.mkToken adaSymbol adaToken

--    cidStart <- Emulator.activateContract (knownWallet 1) ownerEndpoint "start"
--    let pkh = walletPubKeyHash (knownWallet 1)
--    _ <- Emulator.waitNSlots 5
--    us <- Emulator.observableState cidStart >>= \case
--                Monoid.Last (Just (Right v)) -> pure v
--                _                            -> Trace.throwError $ GenericError "initialisation failed"
    cid0 <- Emulator.activateContractWallet (knownWallet 1) userEndpoints
    cid1 <- Emulator.activateContractWallet (knownWallet 2) userEndpoints
    cid2 <- Emulator.activateContractWallet (knownWallet 3) userEndpoints
    cid3 <- Emulator.activateContractWallet (knownWallet 4) userEndpoints
--    _ <- Emulator.waitNSlots 5
    
    let tnNft1,tnNft2,tnNft3,tnNft4 :: BuiltinByteString
        tnNft1 = "CrazyNFT"     --4372617a794e4654"     :: BuiltinByteString --LedgerBytes "CrazyNFT"
        tnNft2 = "CoolNFT"      --"436f6f6c4e4654"       :: BuiltinByteString --LedgerBytes "CoolNFT"
        tnNft3 = "AwesomeNFT"   --"417765736f6d654e4654" :: BuiltinByteString --LedgerBytes --"AwesomeNFT"
        tnNft4 = "StupidNFT"    --"5374757069644e4654"   :: BuiltinByteString --LedgerBytes "StupidNFT"
        csNft :: CurrencySymbol
        -- Have to be matching!
        csNft = "0d78aceeb1526c76b3a13c5713ff3e5fcddac4e0e3e67a3c6483e25a" 

        sp01 = ShopParams {
            ppPrice        = 44000000
          , ppNftCs        = csNft
          , ppNftTn        = TokenName tnNft3 -- fromBytes $ fromBuiltin $ unTokenName 

        }

        sp02 = ShopParams {
            ppPrice        = 35000000
          , ppNftCs        = csNft
          , ppNftTn        = TokenName tnNft2 -- fromBytes $ fromBuiltin $ unTokenName 

        }
    --logInfo @String $ "ShopParams sp01: " ++ show sp01

--    Emulator.callEndpoint @"init" cid0 cpST
    _ <- Emulator.waitNSlots 5

--    Emulator.callEndpoint @"incService" cid0 cpST
--    _ <- Emulator.waitNSlots 5
    
    Emulator.callEndpoint @"create" cid2 sp01
    Emulator.callEndpoint @"create" cid1 sp02
--    Emulator.callEndpoint @"create" cid3 cp3
   -- Emulator.callEndpoint @"create" cid0 cp0
    _ <- Emulator.waitNSlots 5
    Emulator.callEndpoint @"buy" cid3 sp01
    Emulator.callEndpoint @"cancel" cid1 sp02
 --   Emulator.callEndpoint @"buy"    cid2 sp3
   -- Emulator.callEndpoint @"cancel" cid1 cp2
 --   Emulator.callEndpoint @"incService" cid1 cpST2
--    _ <- Emulator.waitNSlots 5
    
 --   _ <- Emulator.waitNSlots 5

    pure ()

-- | Create some sample tokens and distribute them to
--   the emulated wallets
setupTokens :: Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
setupTokens = do
    ownPK <- Plutus.Contract.ownPubKeyHash
   
    logInfo $ "PKH Wallet 1: " ++ show (walletPubKeyHash $ knownWallet 1)
    logInfo $ "PKH Wallet 2: " ++ show (walletPubKeyHash $ knownWallet 2)
    logInfo $ "PKH Wallet 3: " ++ show (walletPubKeyHash $ knownWallet 3)
    logInfo $ "PKH Wallet 4: " ++ show (walletPubKeyHash $ knownWallet 4)
    logInfo $ "PKH Wallet 10: " ++ show (walletPubKeyHash $ knownWallet 10)
    
    cur   <- Currency.mintContract ownPK [(tn, 1) | tn <- tokenNames]
    let cs = Currency.currencySymbol cur

        vl = [(Value.singleton cs tokenName1 1),(Value.singleton cs tokenName2 1),(Value.singleton cs tokenName3 1),(Value.singleton cs tokenName4 1)] 
        v  = mconcat vl
        --v  = mconcat [Value.singleton cs tn amount | tn <- tokenNames]

        w1 = [(knownWallet 1,(vl !! 0)),(knownWallet 2,(vl !! 1)),(knownWallet 3,(vl !! 2)),(knownWallet 4,(vl !! 3))]

    forM_ w1 $ \w -> do
        let pkh = walletPubKeyHash $ (Prelude.fst w)
            v'   = Prelude.snd w
        when (pkh /= ownPK) $ do
            ledgerTx  <- Plutus.Contract.submitTx $ mustPayToPubKey pkh (v')
            either (throwError . Currency.CurContractError . OtherError . pack . show) (awaitTxConfirmed . txId) ledgerTx


    tell $ Just $ Semigroup.Last cur



wallets :: [Wallet]
wallets = take 4 knownWallets

tokenNames :: [TokenName]
tokenNames = ["CrazyNFT", "CoolNFT", "AwesomeNFT", "StupidNFT"]

tokenName1,tokenName2,tokenName3,tokenName4 :: TokenName
tokenName1 = "CrazyNFT"
tokenName2 = "CoolNFT"
tokenName3 = "AwesomeNFT"
tokenName4 = "StupidNFT"

-- ToDo: 
    -- Create some NFTs to the wallets and create suveral shops -> Done
    -- check concurrency works -> Done
    -- implement buy feature -> Done
    -- implement fee in percentage -> Done
    -- implement cancle feature -> Done
    -- implement retrieve ADA from script feature -> 20%
    -- implement PAB testing -> 40%
    -- Add more test Ada to trace -> open
    -- Implement OnChain Validator -> open
