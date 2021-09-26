{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Trinkets2Gold.Smelter(
  trinketAC,

) where

import           Control.Monad        hiding (fmap)
import           Data.List            (find)
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import           Data.Monoid          (Last (..))
import           Data.Text            (Text)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), (<$>), unless, mapMaybe, find)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada hiding (divide)
import           Ledger.Value         as Value
import           Prelude              (Semigroup (..), Show (..), String, (<$>))

import Trinkets2Gold.Mint



data PlayerDatum = PlayerDatum
  {
    playerHash  :: PubKeyHash
  } deriving Show


{-# INLINABLE mkSwapValidator #-}
mkSwapValidator :: AssetClass -> AssetClass -> PlayerDatum -> () -> ScriptContext -> Bool
mkSwapValidator gAC tAC player () ctx =
    (traceIfFalse "expected 15 or more script inputs"          hasEnoughScriptInputs  &&
     traceIfFalse "price not paid or wrong token"              playerInputIsCorrect &&
     traceIfFalse "incorrect amount of gold given"             goldIsGiven            )

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInputValue :: txOutValue
    ownInputValue = txOutValue $ txInInfoResolved $ findOwnInput info

    hasEnoughScriptInputs :: Bool
    hasEnoughScriptInputs = length (txInfoInputs info) >= 15

    playerInputIsCorrect :: Bool
    playerInputIsCorrect = inputTrinketAmount > 0

    inputTrinketAmount :: Integer
    inputTrinketAmount = assetClassValueOf ownInputValue tAC

    -- Due to time constraints, we are assuming that every player has one input and one output associated with them
    goldIsGiven :: Bool
    goldIsGiven = case filter isPlayerOutput $ txnInfoOutputs info of
        [o] | case assetClassValueOf (txOutValue o) gAC == inputTrinketAmount -> True
        _ -> False

    isPlayerOutput :: TxOut -> Bool
    isPlayerOutput out = case toPubKeyHash $ txOutAddress out of
      Just pkh -> phk == playerHash player
      Nothing -> False

data Swapping
Scripts.ValidatorTypes Swapping where
    type instance DatumType Swapping = PlayerDatum
    type instance RedeemerType Swapping = ()

typedSwapValidator :: AssetClass -> AssetClass -> Scripts.TypedValidator Swapping
typedSwapValidator gAC tAC = Scripts.mkTypedValidator @Swapping
    ($$(PlutusTx.compile [|| \gAC' tAC' -> mkSwapValidator gAC' tAC' ||])
        `PlutusTx.applyCode` PlutusTx.liftCode gAC
        `PlutusTx.applyCode` PlutusTx.liftCode tAC)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PlayerDatum @()

swapValidator :: Validator
swapValidator = Scripts.validatorScript . typedSwapValidator

swapAddress :: Ledger.Address
swapAddress = scriptAddress . swapValidator
