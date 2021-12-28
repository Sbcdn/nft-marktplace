{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# options_ghc -fno-specialise         #-}

module OffChain
  where

import           Cardano.Api.Shelley              (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy             as LB
import qualified Data.ByteString.Short            as SBS
import           Ledger                           
import qualified Ledger.Typed.Scripts             as Scripts
import qualified Plutus.V1.Ledger.Scripts         as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude                 
import           Types
import           OnChain                          (vUt)


validator :: BuiltinData -> Scripts.Validator
validator sp = Scripts.mkValidatorScript
        ($$(PlutusTx.compile [|| vUt ||])
        `PlutusTx.applyCode` PlutusTx.liftCode sp)

valInstance :: BuiltinData -> Scripts.TypedValidator Scripts.Any
valInstance sp =
  Scripts.unsafeMkTypedValidator $ validator sp

escrowScript :: BuiltinData -> Validator
escrowScript = Scripts.validatorScript . valInstance

escrowHash :: BuiltinData -> ValidatorHash
escrowHash = Scripts.validatorHash . valInstance

escrowAddress :: BuiltinData -> Ledger.Address 
escrowAddress = Ledger.scriptAddress . escrowScript

scriptAsCbor :: BuiltinData -> LB.ByteString
scriptAsCbor = serialise . escrowScript

apiScript :: ScriptParams -> PlutusScript PlutusScriptV1
apiScript sp = PlutusScriptSerialised $ SBS.toShort $ LB.toStrict $ scriptAsCbor (PlutusTx.toBuiltinData sp)