{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-specialise            #-}

module Types where

import           Ledger
import           Playground.Contract (FromJSON, Generic, ToJSON)
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude             as Prelude
import           Plutus.V1.Ledger.Credential      (Credential)

data ScriptParams = ScriptParams 
        {
              pFee         :: !Integer
            , pAddr        :: !PubKeyHash
        } deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.unstableMakeIsData ''ScriptParams
PlutusTx.makeLift ''ScriptParams

data NftShop = NftShop 
        {
              sPrice        :: !Integer
            , sSeller       :: !PubKeyHash  
            , sRr           :: !Integer
            , sR            :: !PubKeyHash
            , sNftCs        :: !CurrencySymbol
            , sNftTn        :: !TokenName
        } deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.makeIsDataIndexed ''NftShop [('NftShop, 0)]
PlutusTx.makeLift ''NftShop

data Action = Owner | Buy
    deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.makeIsDataIndexed ''Action [('Owner, 1),('Buy, 2)]
PlutusTx.makeLift ''Action

data AAddress = AAddress
  {
      aaddressCredential        :: Credential
    , aaddressStakingCredential :: BuiltinData
  }
PlutusTx.makeIsDataIndexed ''AAddress [('AAddress,0)]

data ATxOut = ATxOut 
  {
      atxOutAddress             :: AAddress
    , atxOutValue               :: Value
    , atxOutDatumHash           :: BuiltinData
  }
PlutusTx.makeIsDataIndexed ''ATxOut [('ATxOut,0)]

data ATxInInfo = ATxInInfo 
  {
      atxInInfoOutRef           :: BuiltinData
    , atxInInfoResolved         :: ATxOut

  }
PlutusTx.makeIsDataIndexed ''ATxInInfo [('ATxInInfo,0)]

data ATxInfo = ATxInfo {
      atxInfoInputs             :: [ATxInInfo]
    , atxInfoOutputs            :: [ATxOut]
    , atxInfoFee                :: BuiltinData
    , atxInfoMint               :: BuiltinData
    , atxInfoDCert              :: BuiltinData
    , atxInfoWdrl               :: BuiltinData
    , atxInfoValidRange         :: BuiltinData
    , atxInfoSignatories        :: [PubKeyHash]
    , atxInfoData               :: BuiltinData
    , atxInfoId                 :: BuiltinData
}
PlutusTx.makeIsDataIndexed ''ATxInfo [('ATxInfo,0)]

data AScriptContext = AScriptContext
  { aScriptContextTxInfo        :: ATxInfo
  , scriptContextPurpose        :: BuiltinData
  }
PlutusTx.makeIsDataIndexed ''AScriptContext [('AScriptContext,0)]
