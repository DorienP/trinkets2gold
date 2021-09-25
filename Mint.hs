{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Trinkets2Gold.Mint where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkGoldPolicy #-}
mkGoldPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkGoldPolicy txORef () ctx = traceIfFalse "expected Gold token" goldIsMinted &&
    traceIfFalse "TxOutRef not found" checkTxOutRef

    where
        info :: TxInfo
        info = txInfo ctx

        goldIsMinted :: Bool
        goldIsMinted = case Value.flattenValue (txInfoMint info) of 
            [(_,tn',amt)] -> tn' == "Gold"
            _ -> False 

        checkTxOutRef :: Bool
        checkTxOutRef = any (\TxInInfo ref _ -> ref == txORef) (txInfoInputs info) 

goldPolicy :: TxOutRef -> Scripts.MintingPolicy
goldPolicy txORef = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkGoldPolicy ||])
    `PlutusTx.applyCode` PlutusTx.liftCode txORef
-- ------------------------------------------------------------------------------------- --

{-# INLINABLE mkTrinketPolicy #-}
mkTrinketPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkTrinketPolicy txORef () ctx = traceIfFalse "expected Trinket token" trinketIsMinted &&
    traceIfFalse "TxOutRef not found" checkTxOutRef

    where
        info :: TxInfo
        info = txInfo ctx

        trinketIsMinted :: Bool
        trinketIsMinted = case Value.flattenValue (txInfoMint info) of 
            [(_,tn',amt)] -> tn' == "Trinket"
            _ -> False 

        checkTxOutRef :: Bool
        checkTxOutRef = any (\TxInInfo ref _ -> ref == txORef) (txInfoInputs info) 
        
trinketPolicy :: TxOutRef -> Scripts.MintingPolicy
trinketPolicy txORef = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkTrinketPolicy ||]) 
    `PlutusTx.applyCode` PlutusTx.liftCode txORef 

-- ------------------------------------------------------------------------------------- --

goldCurSymbol :: TxOutRef -> CurrencySymbol
goldCurSymbol txORef = scriptCurrencySymbol $ goldPolicy txORef

trinketCurSymbol :: TxOutRef -> CurrencySymbol
trinketCurSymbol txORef = scriptCurrencySymbol $ trinketPolicy txORef 

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    , symbolFunc :: TxOutRef -> CurrencySymbol
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

goldParams :: Int -> MintParams
goldParams val = MintParams
    { mpTokenName = "Gold"
    , mpAmount = val
    , symbolFunc = goldCurSymbol
    }

trinketParams :: Int -> MintParams
trinketParams val = MintParams
    { mpTokenName = "Trinket"
    , mpAmount = val
    , symbolFunc = trinketCurSymbol
    }

type MintSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w MintSchema Text ()
mint mp = do
    pk    <- Contract.ownPubKey
    utxos <- utxoAt (pubKeyAddress pk)
    case Map.keys utxos of 
        []      -> Contract.logError @String "no utxo found"
        txORef : _ -> do
            let val     = Value.singleton (symbolFunc txORef) (mpTokenName mp) (mpAmount mp)
                lookups = Constraints.mintingPolicy (goldPolicy txORef)
                    <> Constraints.mintingPolicy (trinketPolicy txORef)
                    <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val
                    <> Constraints.mustSpendPubKeyOutput txORef
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () MintSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''MintSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    let tParams = trinketParams 1000
    let gParams = goldParams 1000
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 tParams
    callEndpoint @"mint" h2 gParams
    void $ Emulator.waitNSlots 1