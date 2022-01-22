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
import           Plutus.V1.Ledger.Credential      (Credential (PubKeyCredential)) --ScriptCredential

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

{-# INLINABLE validateCancel #-}
validateCancel ::  NftShop -> ATxInfo -> Bool
validateCancel NftShop{..} info 
    | txSignedBy' (atxInfoSignatories info) sSeller = True
    | otherwise = False  

{-# INLINABLE minAda' #-}
minAda' :: Integer
minAda' = 1000000

{-# INLINABLE cf #-}
cf :: Integer -> Integer -> Integer 
cf i j = PlutusTx.Prelude.max minAda' (i `PlutusTx.Prelude.divide` 1000 * j)
{-
{-# INLINABLE scriptInputsOk #-}
scriptInputsOk :: [ATxInInfo] -> CurrencySymbol -> TokenName -> Bool
scriptInputsOk i c t
    | length (filter f i) <= 1, length (filter g i) == 1  = True
    | otherwise = False
    where 
        g :: ATxInInfo -> Bool
        g ATxInInfo{atxInInfoResolved=TxOut{txOutAddress=Address (ScriptCredential _) _ , txOutValue}} = valueOf txOutValue c t >= 1
        g _ = False

        f :: ATxInInfo -> Bool
        f ATxInInfo{atxInInfoResolved=TxOut{txOutAddress=Address (ScriptCredential _) _}} = True
        f _ = False
-}
scriptInputsOk :: [ATxInInfo] -> CurrencySymbol -> TokenName -> Bool
scriptInputsOk i c t 
    | length (filter f i) <= 1, length (filter g i) == 1  = True
    | otherwise = False
    where 
        f :: ATxInInfo -> Bool
        f  = isJust . txOutDatumHash . atxInInfoResolved 

        g :: ATxInInfo -> Bool
        g o = valueOf ((txOutValue . atxInInfoResolved) o) c t  >= 1 && f o

{-# INLINABLE txSignedBy' #-}
txSignedBy' :: [PubKeyHash] -> PubKeyHash -> Bool
txSignedBy' txInfoSignatories k =
  isJust $ find (k == ) txInfoSignatories

{-# INLINABLE pubKeyOutputsAt' #-}
pubKeyOutputsAt' :: PubKeyHash -> ATxInfo -> [Value]
pubKeyOutputsAt' pk p =
    let flt TxOut{ txOutAddress = Address (PubKeyCredential pk') _, txOutValue } | pk == pk' = Just txOutValue
                                                                                 | otherwise = Nothing
        flt _                   = Nothing
    in mapMaybe flt (atxInfoOutputs p)

{-# INLINABLE valuePaidTo' #-}
valuePaidTo' :: ATxInfo -> PubKeyHash -> Value
valuePaidTo' info pkh = mconcat (pubKeyOutputsAt' pkh info)

{-# INLINABLE mkVal #-}
mkVal :: ScriptParams -> ShopDatum -> Action -> AScriptContext -> Bool
mkVal sp (Shop d) Buy           ctx   = validateBuy sp d    $ aScriptContextTxInfo ctx
mkVal _  (Shop d) Cancel        ctx   = validateCancel d    $ aScriptContextTxInfo ctx
mkVal _   _       _             _     = False

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