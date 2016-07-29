-- @Author: jonaprieto
-- @Date:   2016-07-29 00:55:59
-- @Last Modified by:   jonaprieto
-- @Last Modified time: 2016-07-29 10:30:03

-- The 3n + 1 problem
module Main
    where

import      System.IO (isEOF)
import      Data.List.Split (splitOn)
import      Control.Monad (unless)

headsnd :: [(Int,Int)] -> Int
headsnd = snd . head

solve :: [Int] -> Int
solve interval = solve' interval []

solve' :: [Int] -> [(Int,Int)] -> Int
solve' [] visited = headsnd visited
solve' (x:xs) visited
    | 0 < fst occur         = max (snd occur) nextVal
    | otherwise             = max (headsnd newvisted) nextVal
    where
        occur  = find x visited
        newvisted = collatz x visited
        nextVal = solve' xs newvisted

find :: Int -> [(Int, Int)] -> (Int,Int)
find x [] = (-1, minBound ::Int )
find x (y:ys)
    | x == fst y            = y
    | otherwise             = find x ys

collatz :: Int -> [(Int,Int)] -> [(Int,Int)]
collatz 1 _ = [(1,1)]
collatz n memo  
    | not (null ans)        = ans
    | n `mod` 2 == 0        = (n, 1 + len1) : headsnd even
    | otherwise             = (n, 1 + len2) : headsnd odd
    where
        ans :: [(Int, Int)]
        ans = dropWhile ((n/=).fst) memo
        even, odd :: [(Int, Int)]
        even  = collatz (n `div` 2) memo
        odd   = collatz (3*n + 1) memo

main :: IO ()
main = do
        end <- isEOF
        unless end $ do
            line <- getLine
            let {ab = map (\x->read x :: Int) (splitOn " " line);
                a = head ab; b = ab !! 1
            } in
                print (solve [a..b])
