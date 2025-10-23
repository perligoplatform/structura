{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Accounts (Account(..),ReserveAmount(..),draw,deposit
                ,transfer,depositInt ,InterestInfo(..),buildEarnIntAction
                ,accBalLens,buildRateResetDates,accrueInt,accTypeLens)
    where
import qualified Data.Time as T
import Stmt (Statement(..),appendStmt,getTxnBegBalance,getDate
            ,TxnComment(..),QueryByComment(..),getTxnComment,getTxnAmt,weightAvgBalanceByDates)
import Types
import Lib
import Util
import DateUtil
import Data.Aeson hiding (json)
import Language.Haskell.TH
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics
import Control.Lens.Tuple
import Control.Lens hiding (Index)
import qualified InterestRate as IR
import qualified Data.DList as DL

-- import Web.Hyperbole

import Debug.Trace
debug = flip trace

data InterestInfo 
  -- | fix reinvest return rate
  = BankAccount IRate DatePattern Date     
  -- | float type: index, spread, sweep dates, rate reset , last accrue day, last reset rate                                 
  | InvestmentAccount Types.Index Spread DatePattern DatePattern Date IRate 
  deriving (Show, Generic, Eq, Ord)

data ReserveAmount 
  -- | target amount with reference to % of formula
  = PctReserve DealStats Rate
  -- | target amount with fixed balance amount                
  | FixReserve Balance     
  -- | target amount depends on a test, if true, then use first one ,otherwise use second one                    
  | Either Pre ReserveAmount ReserveAmount
  -- | use higher of all reserve formulas  
  | Max [ReserveAmount]               
  -- | use lower of all reserve formulas      
  | Min [ReserveAmount]                     
  deriving (Show, Eq, Generic, Ord)

data Account = Account {
    accBalance :: Balance                 -- ^ account current balance
    ,accName :: String                    -- ^ account name
    ,accInterest :: Maybe InterestInfo    -- ^ account reinvestment interest
    ,accType :: Maybe ReserveAmount       -- ^ target info if a reserve account
    ,accStmt :: Maybe Statement           -- ^ transactional history
} deriving (Show, Generic, Eq, Ord)

-- | build interest earn actions
buildEarnIntAction :: [Account] -> Date -> [(String,Dates)] -> [(String,Dates)]
buildEarnIntAction [] ed r = r
buildEarnIntAction (acc:accs) ed r = 
  case accInterest acc of 
    Nothing -> buildEarnIntAction accs ed r
    Just (BankAccount _ dp lastAccDate ) 
      -> buildEarnIntAction accs ed [(accName acc, genSerialDatesTill2 NO_IE lastAccDate dp ed)]++r    
    Just (InvestmentAccount _ _ dp _ lastAccDate _) 
      -> buildEarnIntAction accs ed [(accName acc, genSerialDatesTill2 NO_IE lastAccDate dp ed)]++r    

accrueInt :: Date -> Account -> Balance
accrueInt _ (Account _ _ Nothing _ _) = 0 
-- ^ bank account type interest 
accrueInt endDate a@(Account bal _ (Just interestType) _ stmt)  
  = case stmt of 
      Nothing -> mulBR (mulBI bal rateToUse) (yearCountFraction defaultDc lastDay endDate) -- `debug` (">>"++show lastCollectDate++">>"++show ed)
      Just (Statement txns) ->
        let 
          accrueTxns = sliceBy IE lastDay endDate (DL.toList txns)
          bals = map getTxnBegBalance accrueTxns ++ [bal]
          ds = [lastDay] ++ getDates accrueTxns ++ [endDate]
          avgBal = calcWeightBalanceByDates defaultDc bals ds
        in
          mulBI avgBal rateToUse  
    where 
      defaultDc = DC_30E_360
      (lastDay,rateToUse) = case interestType of 
                              (BankAccount r dp lastCollectDate) -> (lastCollectDate, r)
                              (InvestmentAccount idx spd dp _ lastCollectDate lastRate) -> (lastCollectDate, lastRate)

-- | sweep interest/investement income into account
depositInt :: Date -> Account -> Account
depositInt _ a@(Account _ _ Nothing _ _) = a 
depositInt ed a@(Account bal _ (Just intType) _ stmt)
  = a {accBalance = newBal ,accStmt= appendStmt newTxn stmt ,accInterest = Just (newIntInfoType intType)}
  where 
    accruedInt = accrueInt ed a
    newIntInfoType (BankAccount x y _d) = BankAccount x y ed
    newIntInfoType (InvestmentAccount x y z z1 _d z2) = InvestmentAccount x y z z1 ed z2
    newBal = accruedInt + bal  
    newTxn = AccTxn ed newBal accruedInt BankInt

-- | move cash from account A to account B
transfer :: (Account, Account) -> Date -> Amount -> Either ErrorRep (Account, Account)
transfer (sourceAcc@(Account sBal san _ _ sStmt), targetAcc@(Account tBal tan _ _ tStmt)) d amount
  = do 
      sourceAcc' <- draw d amount (Transfer san tan) sourceAcc
      return (sourceAcc', deposit amount d (Transfer san tan) targetAcc)

-- | deposit cash to account with a comment
deposit :: Amount -> Date -> TxnComment -> Account -> Account
deposit amount d source acc@(Account bal _ _ _ maybeStmt) 
  = let
      newBal = bal + amount
    in
      acc {accBalance = newBal , accStmt = appendStmt (AccTxn d newBal amount source) maybeStmt }

instance Drawable Account where 
  availForDraw d (Account bal _ _ _ _) = ByAvailAmount bal
  draw d 0 txn acc@(Account bal _ _ _ maybeStmt) = return acc 
  draw d amt txn acc@(Account bal _ _ _ maybeStmt) 
    | bal >= amt = return $ deposit (- amt) d txn acc  
    | otherwise = Left  $ "Date:"++ show d ++" Failed to draw "++ show amt ++" from account" ++ accName acc ++ " with balance " ++ show bal


instance QueryByComment Account where 
  queryStmt (Account _ _ _ _ Nothing) tc = []
  queryStmt (Account _ _ _ _ (Just (Statement txns))) tc = filter (\x -> getTxnComment x == tc) (DL.toList txns)


-- InvestmentAccount Types.Index Spread DatePattern DatePattern Date IRate 
buildRateResetDates :: Date -> Account -> Maybe (String,Dates)
buildRateResetDates ed Account{accName = n, accInterest = Just (InvestmentAccount _ _ _ dp sd _) }
  = Just (n, genSerialDatesTill2 NO_IE sd dp ed)
buildRateResetDates _ _ = Nothing


makeLensesFor [("accBalance","accBalLens") ,("accName","accNameLens") 
              ,("accType","accTypeLens") ,("accStmt","accStmtLens"),("accInterest","accIntLens")] ''Account


instance IR.UseRate Account where 
  isAdjustableRate (Account _ an (Just (InvestmentAccount _ _ _ _ _ _)) _ _) = True
  isAdjustableRate _ = False

  getIndex (Account _ an (Just (InvestmentAccount idx _ _ _ _ _)) _ _) = Just idx
  getIndex _ = Nothing 
  

makePrisms ''InterestInfo

$(deriveJSON defaultOptions ''InterestInfo)
$(deriveJSON defaultOptions ''ReserveAmount)
$(deriveJSON defaultOptions ''Account)
