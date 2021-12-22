{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# options_ghc -fno-specialise         #-}
{-# options_ghc -fno-specialise         #-}

module OnChain 
    (mkEscrowValidator)
    where
    
import           Ledger
import           Plutus.V1.Ledger.Value                 
import           Ledger.Ada                       as Ada
import           PlutusTx.Prelude
import           Types


{-# INLINABLE validateBuy #-}
validateBuy :: ScriptParams -> NftShop -> TxInfo -> Bool 
validateBuy ScriptParams{..} NftShop{..} info = isFeePaied && isPricePaied && isNftSend && (oneScript $ txInfoData info)
    where
        buyersKey :: PubKeyHash
        buyersKey = (txInfoSignatories info) !! 0

        fee' :: Integer
        fee' = sPrice `cf` pFee

        price :: Integer
        price = sPrice - fee'

        isPricePaied :: Bool
        isPricePaied = Ada.fromValue (valuePaidTo info sSeller) >= Ada.lovelaceOf price

        isFeePaied :: Bool
        isFeePaied = Ada.fromValue (valuePaidTo info pAddr) >= Ada.lovelaceOf fee'

        isNftSend :: Bool
        isNftSend = checkN info buyersKey sNftCs sNftTn


{-# INLINABLE validateCancel #-}
validateCancel :: ScriptParams -> NftShop -> TxInfo -> Bool
validateCancel ScriptParams{..} NftShop{..} info = isSignedBySeller && isFeePaied && (oneScript $ txInfoData info)
    where
        isSignedBySeller :: Bool
        isSignedBySeller = txSignedBy info sSeller

        fee' :: Integer
        fee' = sPrice `cf` pFee

        fee'' :: Integer
        fee'' = if fee' > 2000000 then 2000000 else fee'

        isFeePaied :: Bool
        isFeePaied = Ada.fromValue (valuePaidTo info pAddr) >= Ada.lovelaceOf fee''


{-# INLINABLE validateUpdate #-}
validateUpdate :: NftShop -> DatumHash -> TxInfo -> Bool
validateUpdate NftShop{..} r info = isSignedBySeller && (isNewDat $ txInfoOutputs info)
    where
        isSignedBySeller :: Bool
        isSignedBySeller = txSignedBy info sSeller

        isNewDat :: [TxOut] -> Bool
        isNewDat [] = False
        isNewDat (o:os) = if (valueOf (txOutValue o) sNftCs sNftTn) == 1 
                            then case txOutDatumHash o of
                                Just dh -> dh == r
                                _       -> False
                            else
                                isNewDat os

{-# INLINABLE cf #-}
cf :: Integer -> Integer -> Integer 
cf i j = o $ i `PlutusTx.Prelude.divide` 1000 * j 
    where
        o :: Integer -> Integer 
        o i' =  if i' < 1000000 then 1000000 else i'

{-# INLINABLE checkN #-}
checkN :: TxInfo -> PubKeyHash -> CurrencySymbol -> TokenName -> Bool
checkN i p c t = case PlutusTx.Prelude.filter (\(c',t',i') -> c' == c && t' == t && i' == 1) (flattenValue (valuePaidTo i p)) of
            [(_,_,_)] -> True
            _        -> False

{-# INLINABLE oneScript #-}
oneScript :: [(DatumHash, Datum)] -> Bool
oneScript [(_,_)]   = True
oneScript _         = False 

{-# INLINABLE mkEscrowValidator #-}
mkEscrowValidator :: ScriptParams -> ShopDatum -> Action -> ScriptContext -> Bool
mkEscrowValidator sp (Shop d) (Buy)      ctx  = validateBuy sp d $ scriptContextTxInfo ctx
mkEscrowValidator sp (Shop d) (Cancel)   ctx  = validateCancel sp d $ scriptContextTxInfo ctx
mkEscrowValidator _  (Shop d) (Update r) ctx  = validateUpdate d r $ scriptContextTxInfo ctx
mkEscrowValidator _   _        _         _    = False 