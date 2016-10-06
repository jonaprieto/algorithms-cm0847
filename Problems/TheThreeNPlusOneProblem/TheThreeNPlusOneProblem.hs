-- | The 3n + 1 Problem

{-# LANGUAGE UnicodeSyntax #-}


module Main
    where

import           Control.Monad   (unless)
import           Data.List.Split (splitOn)
import           System.IO       (isEOF)

headSnd ∷ [(Int, Int)] → Int
headSnd = snd . head

solve ∷ [Int] → Int
solve interval = solve' interval []

solve' ∷ [Int] → [(Int, Int)] → Int
solve' [] visited         = headSnd visited
solve' (x:xs) visited
  | 0 < fst occur         = max (snd occur) nextVal
  | otherwise             = max (headSnd newvisted) nextVal
  where
    occur ∷ (Int,Int)
    occur = find x visited

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
  | n `mod` 2 == 0        = (n, 1 + headSnd evenN) : evenN
  | otherwise             = (n, 1 + headSnd oddN)  : oddN
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
    line ← getLine

    let pair ∷ [Int]
        pair = map read $ splitOn " " line :: [Int]

    let {a = head pair; b = last pair; solution = solve [a..b]} in
      putStrLn $ line ++ " " ++ show solution
    main