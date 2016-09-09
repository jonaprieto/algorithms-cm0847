-- | Shoemaker Problem

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Main
  where

import           Control.Monad   (unless)
import           Data.List       (intercalate, sort)
import           Data.List.Split (splitOn)
import           System.Exit     (exitSuccess)
import           System.IO       (isEOF)



-- 1' : No. del trabajo
-- 2' : Tiempo que se tarda en realizar el pedido
-- 3' : Compensacion por día por cada día de demora

data Order = Order
  { cost  ∷ Int
  , numID ∷ Int
  , time  ∷ Int
  }

type ID = Int

instance Show Order where
  show pedido = concat
    [ show $ numID pedido
    , "\n", show $ time pedido
    , "\n",  show $ cost pedido
    , "\n"
    ]

instance Eq Order where
  p1 == p2 = time p1 == time p2
    && cost p1 == cost p2

  p1 /= p2 = not $ p1 == p2

instance Ord Order where
   p1 <= p2 = (time p1 * cost p2)  <= (time p2 * cost p1)


solve ∷ [Order] → [ID]
solve orders = getIDs $ sort orders
  where
    getIDs ∷ [Order] → [ID]
    getIDs [] = []
    getIDs orders = map numID orders


readOrder ∷ Int → ID → IO [Order]
readOrder 0 _ = return []
readOrder n lastID = do
  ls ← getLine

  let lsnum ∷ [String]
      lsnum = splitOn " " ls

  let showOrder ∷ Order
      showOrder = Order
        { numID = lastID + 1
        , time = read $ head lsnum ∷ Int
        , cost = read $ last lsnum ∷ Int
        }

  otherOrders ∷ [Order] ← readOrder (n-1) (lastID+1)
  return $ showOrder : otherOrders

readCase ∷ Int → IO ()
readCase numCases = do
  end ← isEOF
  unless end $ do
  _ ← getLine

  nn ← getLine

  let n ∷ Int
      n = read nn ∷ Int

  orders ∷ [Order] ← readOrder n 0

  let ids ∷ [ID]
      ids = solve orders

  let ans ∷ String
      ans = intercalate " " $ map show ids

  putStrLn ans
  if numCases > 1 then
    putStr "\n"
  else putStr ""
  readCase (numCases-1)

main ∷ IO ()
main = do
  end ← isEOF
  unless end $ do
    num ← getLine

    let numCases ∷ Int
        numCases = read num ∷ Int

    readCase numCases
