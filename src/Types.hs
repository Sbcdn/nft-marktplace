{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-specialise            #-}


module Types where


--import qualified Data.OpenApi.Schema as OpenApi
import           Ledger
--import           Ledger.Value        (AssetClass (..), assetClass, assetClassValue, assetClassValueOf)
--import           Plutus.V1.Ledger.Bytes           (getLedgerBytes, LedgerBytes)
import           Playground.Contract (FromJSON, Generic, ToJSON, ToSchema, Endpoint)
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude             as Prelude
--import           Text.Printf         (PrintfArg)

----
import qualified Ledger.Typed.Scripts as Scripts
----

{- On Chain Types -}
data ScriptParams = ScriptParams 
        {
              pFee         :: !Integer
            , pAK          :: !PubKeyHash
            , pAddr        :: !PubKeyHash

        } deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.unstableMakeIsData ''ScriptParams
PlutusTx.makeLift ''ScriptParams

--data ShopParams = ShopParams {
--           ppPrice        :: !Integer 
--          , ppNftCs        :: !CurrencySymbol
--          , ppNftTn        :: !TokenName 
--          } deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

data NftShop = NftShop 
        {
              sPrice        :: !Integer
            , sSeller       :: !PubKeyHash  
            , sNftCs        :: !CurrencySymbol
            , sNftTn        :: !TokenName
        } deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.makeIsDataIndexed ''NftShop [('NftShop, 0)]
PlutusTx.makeLift ''NftShop

instance Eq NftShop where
    {-# INLINABLE (==) #-}
    x == y =    sPrice      x == sPrice     y &&
                sSeller     x == sSeller    y &&
                sNftCs      x == sNftCs     y &&
                sNftTn      x == sNftTn     y

data Action = Update DatumHash | Cancel | Buy 
    deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.makeIsDataIndexed ''Action [('Update, 0),('Cancel, 1),('Buy, 2)]
PlutusTx.makeLift ''Action

data ShopDatum = Shop NftShop 
    deriving stock (Prelude.Show, Generic, Prelude.Eq, Prelude.Ord)
PlutusTx.makeIsDataIndexed ''ShopDatum [ ('Shop, 0) ]
PlutusTx.makeLift ''ShopDatum


--data AScriptPurpose = ...

--PlutusTx.makeIsDataIndexed ''AScriptPurpose [...]

--data AScriptContext = AScriptContext
--  { aScriptContextTxInfo :: BuiltinData
  --, scriptContextPurpose :: AScriptPurpose
--  }

--PlutusTx.makeIsDataIndexed ''AScriptContext [('AScriptContext,0)]
