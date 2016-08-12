-- @Author: jonaprieto
-- @Date:   2016-07-29 00:55:59
-- @Last Modified by:   jonaprieto
-- @Last Modified time: 2016-08-11 23:26:01

-- The 3n + 1 problem

module Main
    where

import           Control.Monad   (unless)
import           Data.List.Split (splitOn)
import           System.IO       (isEOF)

headsnd ∷ [(Int, Int)] → Int
headsnd = snd . head

solve ∷ [Int] → Int
solve interval = solve' interval []

solve' ∷ [Int] → [(Int, Int)] → Int
solve' [] visited         = headsnd visited
solve' (x:xs) visited
  | 0 < fst occur         = max (snd occur) nextVal
  | otherwise             = max (headsnd newvisted) nextVal
  where
    occur ∷ (Int,Int)
    occur     = find x visited
    newvisted ∷ [(Int, Int)]
    newvisted = collatz x visited
    nextVal ∷ Int
    nextVal   = solve' xs newvisted

find ∷ Int → [(Int, Int)] → (Int,Int)
find _ [] = (-1, minBound ∷ Int )
find x (y:ys)
  | x == fst y            = y
  | otherwise             = find x ys

collatz ∷ Int → [(Int, Int)] → [(Int, Int)]
collatz 1 _ = [(1,1)]
collatz n memo
  | not (null ans)        = ans
  | n `mod` 2 == 0        = (n, 1 + headsnd evenN) : evenN
  | otherwise             = (n, 1 + headsnd oddN)  : oddN
  where
    ans ∷ [(Int, Int)]
    ans = dropWhile ((n/=).fst) memo
    evenN, oddN ∷ [(Int, Int)]
    evenN  = collatz (n `div` 2) memo
    oddN   = collatz (3*n + 1) memo

main ∷ IO ()
main = do
  end ← isEOF
  unless end $ do
    ab ← getLine
    let a,b ∷ [Int]
        [a,b] = map (\x → read x ∷ Int) (splitOn " " ab)
    putStrLn $ ab ++ " " ++ show $ solve [a..b]
    main
