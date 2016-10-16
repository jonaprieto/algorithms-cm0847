{-# LANGUAGE UnicodeSyntax #-}

-- | Soduku solver

module Main
    where

import           Data.List (nub)


type Matrix = [Int]
type Pos = (Int, Int)


realPos ∷ (Int, Int) → Int
realPos (i,j) = 9 * i + j

change ∷ Matrix → Pos → Int → Matrix
change m p d = (take rpos m) ++ [d] ++ (drop rpos m)
  where
    rpos = realPos p

-- backtrack ∷ Matrix → Pos → Matrix → [Int] → Maybe Matrix
-- backtrack m p@(i,j) currentm ls
--   | length m == length currentm = currentm
--   | not $ inM p                 = Nothing
--   | null ls
--   | m!!(realPos p) > 0          = backtrack m (nextPos p) currentm []
--   | otherwise                   = ans
--   where
--     newm
--     ans = case backtrack m (nextPos p) n ds of
--       Nothing -> if
  --     then case nextPos p of
  --       Just newPos → let newM = backtrack m newPos
  --       _           → Nothing
  --     else
  -- else Nothing


nextPos ∷ Pos → Maybe Pos
nextPos (i,j)
  | j == 8    = Just (i+1, j)
  | i /= 8    = Just (i, j+1)
  | otherwise = Nothing


candidates ∷ Matrix → Pos → [Int]
candidates m p = map (\p → m !! (realPos p)) valid
  where
    valid ∷ [Pos]
    valid = filter (\p → inM p && m!!(realPos p) > 0) positions

    positions ∷ [Pos]
    positions = nub $  perRow ++ perCol ++ perSquare

    perRow ∷ [Pos]
    perRow = posRow m p

    perCol ∷ [Pos]
    perCol = posCol m p

    perSquare ∷ [Pos]
    perSquare = posSquare m p

posRow ∷ Matrix → Pos → [Pos]
posRow m (_,j) = [ (ii, j) | ii ← [0..9] ]

posCol ∷ Matrix → Pos → [Pos]
posCol m (i,_) = [ (i, jj) | jj ← [0..9] ]

posSquare ∷ Matrix → Pos → [Pos]
posSquare m (i,j) = lpos
  where
    lpos ∷ [Pos]
    lpos = [ (3 * ((i-1) `div` 3) + k1, 3 * ((j-1) `div` 3) + k2) | k1 ← [0..3], k2 ← [0..3]]

inM ∷ Pos → Bool
inM (i,j)
  | 0 <= i && i < 9 && 0 <= j && j < 9 = True
  | otherwise = False

main :: IO ()
main = print 1
  -- do
    -- matrix = [[..],[..]]
    -- matrix = concat matrix
    -- backtrack m (0,0)
