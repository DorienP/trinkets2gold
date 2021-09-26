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

trinketName :: TokenName
trinketName = "Trinket"

goldName :: TokenName
goldName :: "Gold"

trinketAC :: AssetClass
trinketAC = AssetClass(trinketCurSymbol, trinketName)

goldAC :: TxOutRef -> AssetClass
goldAC ref = AssetClass(goldCurSymbol ref, goldName)

{-# INLINABLE mkGoldPolicy #-}
mkGoldPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkGoldPolicy txORef () ctx = traceIfFalse "expected Gold token" goldIsMinted &&
    traceIfFalse "TxOutRef not found" checkTxOutRef

    where
        info :: TxInfo
        info = txInfo ctx

        goldIsMinted :: Bool
        goldIsMinted = case Value.flattenValue (txInfoMint info) of
            [(_,tn',amt)] -> tn' == goldName
            _ -> False

        checkTxOutRef :: Bool
        checkTxOutRef = any (\TxInInfo ref _ -> ref == txORef) (txInfoInputs info)

goldPolicy :: TxOutRef -> Scripts.MintingPolicy
goldPolicy txORef = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkGoldPolicy ||])
    `PlutusTx.applyCode` PlutusTx.liftCode txORef
-- ------------------------------------------------------------------------------------- --

{-# INLINABLE mkTrinketPolicy #-}
mkTrinketPolicy :: () -> ScriptContext -> Bool
mkTrinketPolicy () ctx = traceIfFalse "expected Trinket token" trinketIsMinted

    where
        info :: TxInfo
        info = txInfo ctx

        trinketIsMinted :: Bool
        trinketIsMinted = case Value.flattenValue (txInfoMint info) of
            [(_,tn',amt)] -> tn' == trinketName
            _ -> False


trinketPolicy :: Scripts.MintingPolicy
trinketPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkTrinketPolicy ||])


-- ------------------------------------------------------------------------------------- --

goldCurSymbol :: TxOutRef -> CurrencySymbol
goldCurSymbol txORef = scriptCurrencySymbol $ goldPolicy txORef

trinketCurSymbol :: CurrencySymbol
trinketCurSymbol = scriptCurrencySymbol trinketPolicy

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

goldParams :: Int -> MintParams
goldParams val = MintParams
    { mpTokenName = "Gold"
    , mpAmount = val
    }

trinketParams :: Int -> MintParams
trinketParams val = MintParams
    { mpTokenName = "Trinket"
    , mpAmount = val
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
                    <> Constraints.mintingPolicy trinketPolicy
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
