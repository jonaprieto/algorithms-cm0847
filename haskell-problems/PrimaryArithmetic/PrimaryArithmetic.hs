-- | PrimaryArithmetic Problem

{-# LANGUAGE UnicodeSyntax #-}

module Main
    where

import           Control.Monad   (unless)
import           Data.List.Split (splitOn)

countCarries ∷ Int → Int → Int
countCarries n m = countCarries' n m 0 0

countCarries' ∷ Int → Int → Int → Int → Int
countCarries' n m carry carries
  | n == 0              = carries
  | n > m               = countCarries' m n carry carries
  | sumUnits >= 10      = countCarries' newN newM newCarry (carries + 1)
  | otherwise           = countCarries' newN newM 0 carries
  where
    newN, newM, newCarry, sumUnits ∷ Int
    sumUnits = n `mod` 10 + m `mod` 10 + carry
    newN = n `div` 10
    newM = m `div` 10
    newCarry = sumUnits `div` 10

main ∷ IO ()
main = do
  nm ← getLine
  let [n, m] = map (\x → read x ∷ Int) (splitOn " " nm)
  unless ((n+m)== 0) $ do
    case countCarries n m of
      0 → putStrLn "No carry operation."
      1 → putStrLn "1 carry operation."
      c → putStrLn $ show c ++ " carry operations."
    main
