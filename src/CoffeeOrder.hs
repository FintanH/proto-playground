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

-- | An error representing something going wrong during our transaction
--   * 'NotEnoughMoney' represents when a payment cannot fund a 'Coffee' order
--   * 'InvalidPin' a 'CardPayment' contained the wrong PIN
--   * 'NotPreparedForThisPayment' a payment type could not be match with
data TransactionError
  = NotEnoughMoney
  | InvalidPin
  | NotPreparedForThisPayment
  deriving Eq

instance Show TransactionError where
  show = showError
    where
      showError :: TransactionError -> String
      showError NotEnoughMoney =
        "Sorry, you don't have enough money."
      showError InvalidPin =
        "Sorry, you entered your PIN wrong."
      showError NotPreparedForThisPayment =
        "I'm just an intern!"

-- | Smart constructor for making an 'Americano'
--   with a price of €2.70
americano :: P.Coffee
americano =
  def & P.cost      .~ 2.70
      & P.americano .~ def

-- | Smart constructor for making an 'Latte'
--   with a price of €3.20
latte :: P.Coffee
latte =
  def & P.cost  .~ 3.20
      & P.latte .~ def

-- | Smart constructor for making an 'FlatWhite'
--   with a price of €3.30
flatWhite :: P.Coffee
flatWhite =
  def & P.cost      .~ 3.30
      & P.flatWhite .~ def

-- | Smart constructor for making an 'Americano'
--   with a price of €3.00
cappuccino :: P.Coffee
cappuccino =
  def & P.cost       .~ 3.00
      & P.cappuccino .~ def

-- | Smart constructor for making an 'Americano'
--   with a price of €3.50
mocha :: P.Coffee
mocha =
  def & P.cost  .~ 3.50
      & P.mocha .~ def

-- | Process a 'CashPayment' for the total cost of an order
--   It validates that the amount given was sufficient
processCashPayment :: Float
                   -> P.CashPayment
                   -> Either TransactionError ()
processCashPayment amount payment
  | amount <= pay = pure ()
  | amount > pay  = Left NotEnoughMoney
  where
    pay = payment ^. P.amount

-- | Process a 'CardPayment' for the total cost of an order
--   It validates the PIN the customer entered and whether
--   their account contains enough funds
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

-- | Calculate the total cost for a list of coffees
totalCost :: [P.Coffee] -> Float
totalCost = foldr ((+) . view P.cost) 0

-- | Process an 'Order' depending on its payment type
--   Since the 'paymentMethod' is partial due to Protocol Buffers
--   being optional, we must account for the case of not finding
--   a correct payment method
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
    Left err -> show err
    Right _  -> "Next, please!"
