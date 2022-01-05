{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE BangPatterns               #-}
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
import           Plutus.V1.Ledger.Credential (Credential (PubKeyCredential))

{-# INLINABLE validateBuy #-}
validateBuy :: ScriptParams -> NftShop -> ATxInfo -> Bool 
validateBuy ScriptParams{..} NftShop{..} info =                                           
    let 
        fee' :: Integer
        fee' = sPrice `cf` (pFee + sRr)

        price :: Integer
        price = sPrice - fee'

        isPricePaied :: Bool
        isPricePaied = Ada.fromValue (valuePaidTo' info sSeller) >= Ada.lovelaceOf price

        buyersKey :: PubKeyHash
        buyersKey = head (atxInfoSignatories info)

        isNftSend :: Bool
        isNftSend = checkN info buyersKey sNftCs sNftTn    
    in 
        if isFeePaid info pAddr fee' then 
            if isPricePaied then 
                if isNftSend then 
                    if oneScript $ atxInfoData info then
                        True
                    else
                        False
                else
                    False
            else
                False
        else
            False

{-# INLINABLE validateCancel #-}
validateCancel :: ScriptParams -> NftShop -> ATxInfo -> Bool
validateCancel ScriptParams{..} NftShop{..} info 
    | txSignedBy' (atxInfoSignatories info) sSeller, isFeePaid', oneScript $ atxInfoData info = True
    | otherwise = False  
    where
        fee' :: Integer
        fee' = sPrice `cf` pFee

        fee'' :: Integer
        fee'' = if fee' > maxFee then maxFee else fee'

        isFeePaid' :: Bool
        isFeePaid' = Ada.fromValue (valuePaidTo' info pAddr) >= Ada.lovelaceOf fee''


{-# INLINABLE validateUpdate #-}
validateUpdate :: NftShop -> DatumHash -> ATxInfo -> Bool
validateUpdate NftShop{..} r info  
    | txSignedBy' (atxInfoSignatories info) sSeller, (isNewDat $ atxInfoOutputs info) = True
    | otherwise = False  
    where
        isNewDat :: [TxOut] -> Bool
        isNewDat [] = False
        isNewDat (o:os) = if valueOf (txOutValue o) sNftCs sNftTn == 1 
                            then case txOutDatumHash o of
                                Just dh -> dh == r
                                _       -> False
                            else
                                isNewDat os

{-# INLINABLE maxFee #-}
maxFee :: Integer
maxFee = 2000000

{-# INLINABLE minimumFee #-}
minimumFee :: Integer
minimumFee = 1000000

{-# INLINABLE isFeePaid #-}
isFeePaid :: ATxInfo -> PubKeyHash -> Integer -> Bool
isFeePaid info pAddr feeAmt = Ada.fromValue (valuePaidTo' info pAddr) >= Ada.lovelaceOf feeAmt 

{-# INLINABLE cf #-}
cf :: Integer -> Integer -> Integer 
cf i j = o $ i `PlutusTx.Prelude.divide` 1000 * j 
    where
        o :: Integer -> Integer 
        o i' =  if i' < minimumFee then minimumFee else i'

{-# INLINABLE checkN #-}
checkN :: ATxInfo -> PubKeyHash -> CurrencySymbol -> TokenName -> Bool
checkN i p c t =
  let oneToken =
        PlutusTx.Prelude.filter
          (\(c',t',i') -> c' == c && t' == t && i' == 1)
          (flattenValue (valuePaidTo' i p))
  in length oneToken == 1


{-# INLINABLE oneScript #-}
oneScript :: [(DatumHash, Datum)] -> Bool
oneScript l = length l == 1

{-# INLINABLE txSignedBy' #-}
txSignedBy' :: [PubKeyHash] -> PubKeyHash -> Bool
txSignedBy' txInfoSignatories k =
  isJust $ find (k == ) txInfoSignatories

{-# INLINABLE pubKeyOutputsAt' #-}
pubKeyOutputsAt' :: PubKeyHash -> ATxInfo -> [Value]
pubKeyOutputsAt' pk p =
    let flt TxOut{ txOutAddress = Address (PubKeyCredential pk') _, txOutValue } | pk == pk' = Just txOutValue
                                                                                 | otherwise = Nothing
        flt _                             = Nothing
    in mapMaybe flt (atxInfoOutputs p)

{-# INLINABLE valuePaidTo' #-}
valuePaidTo' :: ATxInfo -> PubKeyHash -> Value
valuePaidTo' info pkh = mconcat (pubKeyOutputsAt' pkh info)

{-# INLINABLE mkVal #-}
mkVal :: ScriptParams -> ShopDatum -> Action -> AScriptContext -> Bool
mkVal sp (Shop d) Buy           ctx   = validateBuy sp d $ aScriptContextTxInfo ctx
mkVal sp (Shop d) Cancel        ctx   = validateCancel sp d $ aScriptContextTxInfo ctx
mkVal _  (Shop d) (Update dh)   ctx   = validateUpdate d dh $ aScriptContextTxInfo ctx

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