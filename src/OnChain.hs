{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# options_ghc -fno-specialise         #-}

module OnChain 
    (vUt)
    where
    
import           Ledger
import           Plutus.V1.Ledger.Value        
import           Ledger.Ada                       as Ada
import           PlutusTx.Prelude
import           Types
import           PlutusTx.IsData.Class
import           Plutus.V1.Ledger.Credential (Credential (PubKeyCredential))

{-# INLINABLE validateBuy #-}
validateBuy :: ScriptParams -> NftShop -> ATxInfo -> Bool 
validateBuy ScriptParams{..} NftShop{..} info = 
                                                if isFeePaied then 
                                                    if isPricePaied then 
                                                        if isNftSend then 
                                                            if (oneScript $ atxInfoData info) then
                                                                True
                                                            else
                                                               False
                                                        else
                                                            False
                                                    else
                                                        False
                                                else
                                                    False
    where
        buyersKey :: PubKeyHash
        buyersKey = (atxInfoSignatories info) !! 0

        fee' :: Integer
        fee' = sPrice `cf` pFee

        price :: Integer
        price = sPrice - fee'

        isPricePaied :: Bool
        isPricePaied = Ada.fromValue (valuePaidTo' info sSeller) >= Ada.lovelaceOf price

        isFeePaied :: Bool
        isFeePaied = Ada.fromValue (valuePaidTo' info pAddr) >= Ada.lovelaceOf fee'

        isNftSend :: Bool
        isNftSend = checkN info buyersKey sNftCs sNftTn


{-# INLINABLE validateCancel #-}
validateCancel :: ScriptParams -> NftShop -> ATxInfo -> Bool
validateCancel ScriptParams{..} NftShop{..} info = 
                                                    if txSignedBy' (atxInfoSignatories info) sSeller then 
                                                        if isFeePaied then 
                                                            if (oneScript $ atxInfoData info) then
                                                                True
                                                            else
                                                                False
                                                        else
                                                            False
                                                    else 
                                                        False
    where

        fee' :: Integer
        fee' = sPrice `cf` pFee

        fee'' :: Integer
        fee'' = if fee' > 2000000 then 2000000 else fee'

        isFeePaied :: Bool
        isFeePaied = Ada.fromValue (valuePaidTo' info pAddr) >= Ada.lovelaceOf fee''


{-# INLINABLE validateUpdate #-}
validateUpdate :: NftShop -> DatumHash -> ATxInfo -> Bool
validateUpdate NftShop{..} r info = 
                                        if txSignedBy' (atxInfoSignatories info) sSeller then 
                                            if (isNewDat $ atxInfoOutputs info) then
                                                True
                                            else
                                                False
                                        else 
                                            False
    where
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
checkN :: ATxInfo -> PubKeyHash -> CurrencySymbol -> TokenName -> Bool
checkN i p c t = case PlutusTx.Prelude.filter (\(c',t',i') -> c' == c && t' == t && i' == 1) (flattenValue (valuePaidTo' i p)) of
            (_:[])   -> True
            _        -> False

{-# INLINABLE oneScript #-}
oneScript :: [(DatumHash, Datum)] -> Bool
oneScript (_:[])     = True
oneScript  _         = False 

{-# INLINABLE txSignedBy' #-}
txSignedBy' :: [PubKeyHash] -> PubKeyHash -> Bool
txSignedBy' txInfoSignatories k = case find ((==) k) txInfoSignatories of
    Just _  -> True
    Nothing -> False

{-# INLINABLE pubKeyOutputsAt' #-}
pubKeyOutputsAt' :: PubKeyHash -> ATxInfo -> [Value]
pubKeyOutputsAt' pk p =
    let flt TxOut{txOutAddress = Address (PubKeyCredential pk') _, txOutValue} | pk == pk' = Just txOutValue
        flt _                             = Nothing
    in mapMaybe flt (atxInfoOutputs p)

{-# INLINABLE valuePaidTo' #-}
valuePaidTo' :: ATxInfo -> PubKeyHash -> Value
valuePaidTo' ptx pkh = mconcat (pubKeyOutputsAt' pkh ptx)

{-# INLINABLE mkVal #-}
mkVal :: ScriptParams -> ShopDatum -> Action -> AScriptContext -> Bool
mkVal sp (Shop d) (Buy)       ctx  = validateBuy sp d $ aScriptContextTxInfo ctx
mkVal sp (Shop d) (Cancel)    ctx  = validateCancel sp d $ aScriptContextTxInfo ctx
mkVal _  (Shop d) (Update dh) ctx  = validateUpdate d dh $ aScriptContextTxInfo ctx

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