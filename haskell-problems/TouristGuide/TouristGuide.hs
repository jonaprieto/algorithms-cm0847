{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Main
  where

import           Control.Monad   (unless)
import           Data.List.Split (splitOn)
import           System.Exit     (exitSuccess)
import           System.IO       (isEOF)

import           Data.List       (sort)
import           Data.Set        (Set, fromList, insert, member, union)

data Edge = Edge Int Int Int

instance Show Edge where
  show (Edge _ _ e) = show e ++ "\n"

instance Eq (Edge) where
  Edge x1 y1 z1 == Edge x2 y2 z2 = x1 == x2 && y1 == y2 && z1 == z2

instance Ord (Edge) where
  (Edge _ _ x) <= (Edge _ _ y) = x <= y

kruskal :: [Edge] -> [Edge]
kruskal = fst . foldl mst ([],[]) . sort

mst :: ([Edge],[Set Int]) -> Edge -> ([Edge],[Set Int])
mst (es, sets) e@(Edge p q _) = step $ extract sets where
   step (rest, Nothing, Nothing) = (e : es, fromList [p,q] : rest)
   step (rest, Just ps, Nothing) = (e : es, q `insert` ps : rest)
   step (rest, Nothing, Just qs) = (e : es, p `insert` qs : rest)
   step (rest, Just ps, Just qs) | ps == qs = (es, sets) --circle
                                 | otherwise = (e : es, ps `union` qs : rest)
   extract = foldr f ([], Nothing, Nothing) where
       f s (list, setp, setq) =
            let list' = if member p s || member q s then list else s:list
                setp' = if member p s then Just s else setp
                setq' = if member q s then Just s else setq
            in (list', setp', setq')

readEdge ∷ IO Edge
readEdge = do
  ln ← getLine
  let from, go, weight ∷ Int
      [from, go, weight] = map read $ splitOn " " ln

  let edge ∷ Edge
      edge = Edge from go (-1 * (weight - 1) )

  return edge

minEdge ∷ [Edge] → Int
minEdge [] = maxBound ∷ Int
minEdge (Edge _ _ w : rest) = min w $ minEdge rest

readCase ∷ Int → IO ()
readCase nCase = do
  end ← isEOF
  unless end $ do
    ln ← getLine

    let n, r ∷ Int
        [n, r] = map read $ splitOn " " ln

    unless (n==0 && r == 0) $ return ()

    edges ∷ [Edge] ← mapM (\_→ readEdge) $ replicate r 1

    end ← isEOF
    unless end $ do

      ln ← getLine
      unless (nCase == 1) $ putStr "\n"

      let source, goal, tourists ∷ Int
          [source, goal, tourists] = map read $ splitOn " " ln

      let mst ∷ [Edge]
          mst = kruskal edges
      -- print "arbol"
      -- print edges
      -- print "mst"
      -- print mst

      let minWeight ∷ Int
          minWeight = -1 * minEdge mst

      -- print tourists
      -- print minWeight

      let ans ∷ Int
          ans = if minWeight > 0
            then (tourists + minWeight - 1) `div` minWeight
            else 0

      putStrLn $ "Scenario #" ++ show nCase
      putStrLn $ "Minimum Number of Trips = " ++ show ans
      readCase $ nCase + 1

main ∷ IO ()
main = do
  end ← isEOF
  unless end $ do
    readCase 1

