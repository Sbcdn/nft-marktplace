{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# OPTIONS_GHC -fno-specialise         #-}

module OnChain 
    ( vUt
    ) where
    
import           Ledger
import           Plutus.V1.Ledger.Value        
import           Ledger.Ada                       as Ada
import           PlutusTx.Prelude
import           Types
import           PlutusTx.IsData.Class
import           Plutus.V1.Ledger.Credential      (Credential (PubKeyCredential, ScriptCredential))

{-# INLINABLE validateBuy #-}
validateBuy :: ScriptParams -> NftShop -> ATxInfo -> Bool 
validateBuy ScriptParams{..} NftShop{..} info 
    | isPaid pAddr fee', isPaid sSeller price, sRr <= 0 || isPaid sR roy, isNftSend, scriptInputsOk (atxInfoInputs info) sNftCs sNftTn = True
    | otherwise = False              
    where 
        fee' :: Integer
        fee' = sPrice `cf` pFee

        roy :: Integer
        roy = if sRr <= 0 then 0 else sPrice `cf` sRr

        price :: Integer
        price = sPrice - fee' - roy

        isPaid :: PubKeyHash -> Integer -> Bool
        isPaid addr amt = Ada.fromValue (valuePaidTo' info addr) >= Ada.lovelaceOf amt 

        isNftSend :: Bool
        isNftSend = valueOf (valuePaidTo' info (head $ atxInfoSignatories info)) sNftCs sNftTn >= 1

{-# INLINABLE validateOwner #-}
validateOwner ::  NftShop -> ATxInfo -> Bool
validateOwner NftShop{..} info 
    | txSignedBy' (atxInfoSignatories info) sSeller = True
    | otherwise = False  

{-# INLINABLE minAda' #-}
minAda' :: Integer
minAda' = 1000000

{-# INLINABLE cf #-}
cf :: Integer -> Integer -> Integer 
cf i j = PlutusTx.Prelude.max minAda' (i `PlutusTx.Prelude.divide` 1000 * j)

{-# INLINABLE isScriptAddress #-}
isScriptAddress :: AAddress -> Bool
isScriptAddress AAddress { aaddressCredential } = case aaddressCredential of
  ScriptCredential _    -> True
  _                     -> False

{-# INLINABLE scriptInputsOk #-}
scriptInputsOk :: [ATxInInfo] -> CurrencySymbol -> TokenName -> Bool
scriptInputsOk i c t =
  let
    isScriptInput :: ATxInInfo -> Bool
    isScriptInput = isScriptAddress . atxOutAddress . atxInInfoResolved

  in case filter isScriptInput i of
    [ATxInInfo{atxInInfoResolved=ATxOut{atxOutValue=v}}] -> valueOf v c t >= 1
    _ ->  False

{-# INLINABLE txSignedBy' #-}
txSignedBy' :: [PubKeyHash] -> PubKeyHash -> Bool
txSignedBy' atxInfoSignatories k =
  isJust $ find (k == ) atxInfoSignatories

{-# INLINABLE pubKeyOutputsAt' #-}
pubKeyOutputsAt' :: PubKeyHash -> ATxInfo -> [Value]
pubKeyOutputsAt' pk p =
    let flt ATxOut{ atxOutAddress = AAddress (PubKeyCredential pk') _, atxOutValue } | pk == pk' = Just atxOutValue
                                                                                     | otherwise = Nothing
        flt _                     = Nothing
    in mapMaybe flt (atxInfoOutputs p)

{-# INLINABLE valuePaidTo' #-}
valuePaidTo' :: ATxInfo -> PubKeyHash -> Value
valuePaidTo' info pkh = mconcat (pubKeyOutputsAt' pkh info)

{-# INLINABLE mkVal #-}
mkVal :: ScriptParams -> NftShop -> Action -> AScriptContext -> Bool
mkVal sp d Buy           ctx   = validateBuy sp d    $ aScriptContextTxInfo ctx
mkVal _  d Owner         ctx   = validateOwner  d    $ aScriptContextTxInfo ctx

{-# INLINABLE vUt #-}
vUt :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
vUt s d r c = 
   wVal mkVal (unsafeFromBuiltinData s) (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c)

{-# INLINABLE wVal #-}
wVal :: forall s d r c
    . (UnsafeFromData s, UnsafeFromData d, UnsafeFromData r, UnsafeFromData c)
    => (s -> d -> r -> c -> Bool)
    -> BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> ()
wVal f s d r c = check (f (unsafeFromBuiltinData s) (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c))