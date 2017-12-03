{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

module Main where

import           Data.Default
import           Data.String (IsString)
import           Data.Text (Text)
import           Lens.Micro
import           Lens.Micro.Extras (view)
import qualified Proto.Coffee.Order as P
import qualified Proto.Coffee.Order'Fields as P

data TransactionError
  = NotEnoughMoney
  | InvalidPin
  | NotPreparedForThisPayment

newtype PIN = PIN Text
  deriving (IsString)

showError :: TransactionError -> String
showError NotEnoughMoney =
  "Sorry, you don't have enough money."
showError InvalidPin =
  "Sorry, you entered your PIN wrong."
showError NotPreparedForThisPayment =
  "I'm just an intern!"

americano :: P.Coffee
americano =
  def & P.cost      .~ 2.70
      & P.americano .~ def

latte :: P.Coffee
latte =
  def & P.cost  .~ 3.20
      & P.latte .~ def

flatWhite :: P.Coffee
flatWhite =
  def & P.cost      .~ 3.30
      & P.flatWhite .~ def

cappuccino :: P.Coffee
cappuccino =
  def & P.cost       .~ 3.00
      & P.cappuccino .~ def

mocha :: P.Coffee
mocha =
  def & P.cost  .~ 3.50
      & P.mocha .~ def

processCashPayment :: Float
                   -> P.CashPayment
                   -> Either TransactionError ()
processCashPayment amount payment
  | amount <= pay = pure ()
  | amount > pay  = Left NotEnoughMoney
  where
    pay = payment ^. P.amount

processCardPayment :: Float
                   -> P.CardPayment
                   -> Either TransactionError ()
processCardPayment amount payment =
  pinCheck *> balanceCheck
  where
    account = payment ^. P.account
    pinCheck
      | account ^. P.pinValidation == payment ^. P.pin = pure ()
      | otherwise = Left InvalidPin

    balanceCheck
      | account ^. P.currentBalance >= amount = pure ()
      | otherwise = Left NotEnoughMoney

totalCost :: [P.Coffee] -> Float
totalCost = foldr ((+) . view P.cost) 0

takeOrder :: Float
          -> P.Order
          -> Either TransactionError ()
takeOrder amount order =
  case order ^. P.maybe'paymentMethod of
    Just (P.Order'Card card) -> processCardPayment amount card
    Just (P.Order'Cash cash) -> processCashPayment amount cash
    _                        -> Left NotPreparedForThisPayment

main :: IO ()
main = do
  putStrLn "How can I help you?"
  putStrLn "Two americanos and a flat white please :)"

  let order1Coffees = [americano, americano, flatWhite]
      totalCost1    = totalCost order1Coffees

  putStrLn $ "That's €" ++ show totalCost1 ++ ". Will that be cash or card?"
  putStrLn "Cash, please"

  let order1 :: P.Order
      order1 =
        def & P.coffees .~ [americano, americano, flatWhite]
            & P.cash    .~ (def & P.amount .~ totalCost1)

  putStrLn $ case takeOrder totalCost1 order1 of
    Left err -> showError err
    Right _  -> "Next, please!"
