{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# options_ghc -fno-specialise         #-}

module OffChain
  where
import           Cardano.Api.Shelley            (PlutusScript (..),
                                                 PlutusScriptV2)
import           Codec.Serialise
import qualified Data.ByteString.Lazy           as LB
import qualified Data.ByteString.Short          as SBS
import qualified Ledger
import qualified Plutus.Script.Utils.V2.Scripts as Utils
import qualified Plutus.V1.Ledger.Address       as Address
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude
import           OnChain                        (vUt)
import           Types                          (ScriptParams (..))

validator :: BuiltinData -> PlutusV2.Validator
validator sp = PlutusV2.mkValidatorScript
        ($$(PlutusTx.compile [|| vUt ||])
        `PlutusTx.applyCode` PlutusTx.liftCode sp)

script :: BuiltinData -> PlutusV2.Script
script = PlutusV2.unValidatorScript . validator

scriptHash :: BuiltinData -> PlutusV2.ValidatorHash
scriptHash = Utils.validatorHash . validator

scriptAddress :: ScriptParams -> Ledger.Address
scriptAddress sp = Address.scriptHashAddress $ scriptHash $ PlutusTx.toBuiltinData sp

scriptAsCbor :: BuiltinData -> LB.ByteString
scriptAsCbor = serialise . script

apiScript :: ScriptParams -> PlutusScript PlutusScriptV2
apiScript sp = PlutusScriptSerialised $ SBS.toShort $ LB.toStrict $ scriptAsCbor (PlutusTx.toBuiltinData sp)