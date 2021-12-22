{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module OffChain
  where

{- Serialize Plutus Script -}
import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import qualified Plutus.V1.Ledger.Scripts as Plutus

import           Control.Monad                    (forever)
import           Control.Lens                     (view)
import           Plutus.Contract.Request          as Request
import           Ledger
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import           Ledger.Constraints               as Constraints
import           Plutus.V1.Ledger.Value           --(TokenName ,AssetClass (..), symbols, singleton)
import           Plutus.V1.Ledger.Bytes           (getLedgerBytes, LedgerBytes, fromHex)
import           Playground.Contract
import           Plutus.Contract
import           Data.Text                        (Text, pack)
import qualified Data.Map                         as Map
import           Text.Printf                      (printf)
import           Schema
import           Ledger.Ada


import           Data.Proxy                       (Proxy (..))
import           Ledger                           hiding (singleton)
import qualified Ledger.Typed.Scripts             as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), dropWhile, flip, unless, forever)
import           Prelude                          (Int, Semigroup (..), String, div, dropWhile, flip, show, 
                                                              (^))
import           Types
import           OnChain                          (mkEscrowValidator)


{- Off Chain Types -}

data Validating
instance Scripts.ValidatorTypes Validating where
        type instance RedeemerType Validating = Action
        type instance DatumType    Validating = ShopDatum


valInstance :: ScriptParams -> Scripts.TypedValidator Validating
valInstance sp = Scripts.mkTypedValidator @Validating
        ($$(PlutusTx.compile [|| mkEscrowValidator ||])
            `PlutusTx.applyCode` PlutusTx.liftCode sp)
        $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @ShopDatum @Action

escrowScript :: ScriptParams -> Validator
escrowScript = Scripts.validatorScript . valInstance

escrowHash :: ScriptParams -> ValidatorHash
escrowHash = Scripts.validatorHash . valInstance

escrowAddress :: ScriptParams -> Ledger.Address 
escrowAddress = Ledger.scriptAddress . escrowScript

scriptAsCbor :: ScriptParams -> LB.ByteString
scriptAsCbor = serialise . escrowScript

apiScript :: ScriptParams -> PlutusScript PlutusScriptV1
apiScript = PlutusScriptSerialised . SBS.toShort . LB.toStrict . scriptAsCbor


{- Trace Testing -}

{--

-- | Creates a NFT sale
create :: forall w s. ShopParams -> Contract w s Text ()
create ShopParams{..} = do
    pkh <- Request.ownPubKeyHash

    let shop = NftShop {
              sPrice        = ppPrice
            , sSeller       = pkh 
            , sNftCs        = ppNftCs
            , sNftTn        = ppNftTn
            }
        sp = ScriptParams {
              pFee         = 150
            , pCancelFee   = 2000000
            , pAddr        = "a96a668ed7be83e332c872f51da7925b4472ca98c4f517efa4bbb9fb" ::PubKeyHash
          }

    let inst   = valInstance sp
        ecScript = escrowScript sp
        nft = unitValue $ AssetClass (ppNftCs, ppNftTn)  --,TokenName ppNftTn)
            --(TokenName $ either (error()) (getLedgerBytes) (fromHex $ fromBuiltin ppNftTn))
    
    logInfo $ "created NFT Shop: " ++ show shop
    let lookups  = Constraints.typedValidatorLookups inst                                   <>
                   Constraints.otherScript ecScript                                         

        tx       = Constraints.mustPayToTheScript (Shop shop) nft                                

    ledgerTx <- submitTxConstraintsWith lookups tx
    tx <- case ledgerTx of 
        Left a -> throwError ""
        Right b -> awaitTxConfirmed $ txId b
    --void $ awaitTxConfirmed $ txId ledgerTx

    utxos <- utxosAt $ escrowAddress sp
    logInfo $ "----------------------------------------Spacer--------------------------------" ++ ""
    logInfo $ "Utxos At: " ++ show (utxos)
    logInfo $ "----------------------------------------Spacer--------------------------------" ++ ""
    logInfo $ "created NFT Shop: " ++ show shop


-- | Cancel a NFT sale
cancel :: forall w s. ShopParams -> Contract w s Text ()
cancel params@ShopParams{..} = do
    pkh <- Request.ownPubKeyHash

    let sp = ScriptParams {
              pFee         = 150
            , pCancelFee   = 2000000
            , pAddr        = "a96a668ed7be83e332c872f51da7925b4472ca98c4f517efa4bbb9fb" ::PubKeyHash
          }

    let inst   = valInstance sp
        ecScript = escrowScript sp
        asset = AssetClass (ppNftCs, ppNftTn)  -- ,TokenName ppNftTn)
        nft = unitValue asset

    (oref,o,s) <- findShop sp asset
      
    let lookups  = Constraints.typedValidatorLookups inst                                   <>
                   Constraints.otherScript ecScript                                         <>
                   Constraints.unspentOutputs (Map.singleton oref o)                                 

        tx       = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Cancel) <>                               
                   Constraints.mustPayToPubKey (sSeller s) nft                                                 <>
                   Constraints.mustPayToPubKey (pAddr sp) (lovelaceValueOf $ pCancelFee sp)                 <>
                   Constraints.mustBeSignedBy  (sSeller s)

    ledgerTx <- submitTxConstraintsWith lookups tx
    tx <- case ledgerTx of 
        Left a -> throwError ""
        Right b -> awaitTxConfirmed $ txId b

    utxos <- utxosAt $ escrowAddress sp
    logInfo $ "----------------------------------------Spacer--------------------------------" ++ ""
    logInfo $ "Utxos At: " ++ show (utxos)
    logInfo $ "----------------------------------------Spacer--------------------------------" ++ ""
    logInfo $ "Canceled NFT Shop: " ++ show s


-- | Cancel a NFT sale
buy :: forall w s. ShopParams -> Contract w s Text ()
buy params@ShopParams{..} = do
    pkh <- Request.ownPubKeyHash

    let sp = ScriptParams {
              pFee         = 150
            , pCancelFee   = 2000000
            , pAddr        = "a96a668ed7be83e332c872f51da7925b4472ca98c4f517efa4bbb9fb" ::PubKeyHash
          }

    let inst   = valInstance sp
        ecScript = escrowScript sp
        asset = AssetClass (ppNftCs, ppNftTn)  --,TokenName ppNftTn)
        nft = unitValue asset
        

    (oref,o,s) <- findShop sp asset

    let fee   = (sPrice s) `PlutusTx.Prelude.divide` 1000 * (pFee sp)
        price = ((sPrice s) - fee)

    let lookups  = Constraints.typedValidatorLookups inst                                   <>
                   Constraints.otherScript ecScript                                         <>
                   Constraints.unspentOutputs (Map.singleton oref o)                                 

        tx       = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Buy)       <>                               
                   Constraints.mustPayToPubKey (sSeller s) (lovelaceValueOf price)                        <>
                   Constraints.mustPayToPubKey (pAddr sp) (lovelaceValueOf fee)                           <>
                   Constraints.mustPayToPubKey pkh nft                                                    


    ledgerTx <- submitTxConstraintsWith lookups tx
    tx <- case ledgerTx of 
        Left a -> throwError ""
        Right b -> awaitTxConfirmed $ txId b

    utxos <- utxosAt $ escrowAddress sp
    logInfo $ "----------------------------------------Spacer--------------------------------" ++ ""
    logInfo $ "Utxos At: " ++ show (utxos)
    logInfo $ "----------------------------------------Spacer--------------------------------" ++ ""
    logInfo $ "NFT Bought: " ++ show s




getEscrowDatum :: ChainIndexTxOut -> Contract w s Text NftShop
getEscrowDatum o =
  case o of
      PublicKeyChainIndexTxOut {} ->
        throwError "no datum for a txout of a public key address"
      ScriptChainIndexTxOut { _ciTxOutDatum } -> do
        (Datum e) <- either getDatum pure _ciTxOutDatum
        maybe (throwError "datum hash wrong type")
              pure
              (PlutusTx.fromBuiltinData e)
  where
    getDatum :: DatumHash -> Contract w s Text Datum
    getDatum dh =
      datumFromHash dh >>= \case Nothing -> throwError "datum not found"
                                 Just d  -> pure d


findNft ::
    forall a w s.
    ScriptParams
    -> AssetClass
    -> (NftShop -> Maybe a)
    -> Contract w s Text (TxOutRef, ChainIndexTxOut, a)
findNft sp c f = do
    let addr = escrowAddress sp
    logInfo $ "#" ++ ""
    logInfo $ "----------------------------------------Spacer--------------------------------" ++ ""
    logInfo @String $ printf "looking for NFT Shop at address %s containing token %s " (show addr) (show c)
    utxos <- utxosAt addr
    logInfo $ "----------------------------------------Spacer--------------------------------" ++ ""
    logInfo $ "Utxos At: " ++ show (utxos)
    logInfo $ "----------------------------------------Spacer--------------------------------" ++ ""
    logInfo $ "----------------------------------------Spacer--------------------------------" ++ ""
    go [x | x@(_, o) <- Map.toList utxos, isUnity (view ciTxOutValue o) c]

  where
    go [] = throwError "No Shop for NFT found"
    go ((oref,o) : xs) = do
        d <- getEscrowDatum o
        case f d of
            Nothing -> go xs
            Just a  -> do
                logInfo @String $ printf "found Nft Shop with datum: %s" (show o)
                logInfo $ "----------------------------------------Spacer--------------------------------" ++ ""
                logInfo $ "#" ++ ""
                return (oref, o, a)

findShop :: forall b w s. ScriptParams -> AssetClass -> Contract w s Text (TxOutRef, ChainIndexTxOut, NftShop)
findShop sp nft = findNft sp nft $ \case
    lps -> Just lps
    _   -> Nothing


type GamblingUserSchema = 
            Endpoint "create"     ShopParams
        .\/ Endpoint "cancel"     ShopParams
        .\/ Endpoint "buy"        ShopParams

userEndpoints :: Contract () GamblingUserSchema Text ()
userEndpoints = forever 
                $ handleError logError
                $ awaitPromise
                $ create' `select` cancel' `select` buy'
    where
        create'  = endpoint @"create" create
        cancel'  = endpoint @"cancel" cancel
        buy'     = endpoint @"buy"    buy 

-- TokenName $ either (error()) (getLedgerBytes) (fromHex $ fromBuiltin tn)
--}