
-- | The Tourist Guide Problem

{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}


module Main
  where

import           Control.Monad   (unless)
import           Data.List.Split (splitOn)
import           System.Exit     (exitSuccess)
import           System.IO       (isEOF)

import           Data.List       (nub, sort)
import           Data.Set        (Set, fromList, insert, member, union)

data Edge = Edge Int Int Int

instance Show Edge where
  show (Edge a b e) = show a ++ " → " ++ show b ++ " : " ++ show e ++ "\n"

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
      edge = Edge from go $ -1 * weight

  return edge

inNode ∷ Edge → Int
inNode (Edge a _ _ ) = a

outNode ∷ Edge → Int
outNode (Edge _ b _ ) = b

weightEdge ∷ Edge → Int
weightEdge (Edge _ _ w) = w

-- all edges that out from source S taking into
-- account the viseted nodes
inS ∷ [Edge] → [Int] → Int → [Edge]
inS xs visited s =
  [ edge | edge ← xs,
    s == inNode edge,
    not $ outNode edge `elem` visited
  ]

weightsPaths ∷ [Edge] → [Int] → Int → Int → [[Int]]
weightsPaths xs visited s t
  | s == t = [[]]
  | null ins = [[minBound∷Int]]
  | otherwise = paths'
  where
    ins ∷ [Edge]
    ins = inS xs visited s

    paths ∷ Edge → [[Int]]
    paths edge = weightsPaths xs (s:visited) (outNode edge) t

    paths' ∷ [[Int]]
    paths' = [ (weightEdge edge):p | edge ← ins, p ← paths edge]

readCase ∷ Int → IO ()
readCase nCase = do
  end ← isEOF
  unless end $ do
    ln ← getLine

    let n, r ∷ Int
        [n, r] = map read $ splitOn " " ln

    unless (n == 0 && r == 0) $ return ()

    edges ∷ [Edge] ← mapM (\_→ readEdge) $ replicate r 1

    end ← isEOF
    unless end $ do

      ln ← getLine
      unless (nCase == 1) $ putStr "\n"

      let source, goal, tourists ∷ Int
          [source, goal, tourists] = map read $ splitOn " " ln

      let mst ∷ [Edge]
          mst = map (\(Edge a b w) → Edge a b (-1*w)) $ kruskal edges

      let tree ∷ [Edge]
          tree = nub $ mst  ++ map (\(Edge a b w)→ Edge b a w) mst

      let weightspaths ∷ [[Int]]
          weightspaths = weightsPaths tree [] source goal

      let minEdges ∷ [Int]
          minEdges = [ if null ws then minBound ∷ Int else minimum ws | ws ← weightspaths]

      let minWeight ∷ Int
          minWeight = (maximum minEdges) - 1

      let ans ∷ Int
          ans =
            if | tourists == 0   → 0
               | source == goal  → 1
               | otherwise       →
                  (tourists + minWeight - 1) `div` minWeight

      putStrLn $ "Scenario #" ++ show nCase
      putStrLn $ "Minimum Number of Trips = " ++ show ans
      readCase $ nCase + 1

main ∷ IO ()
main = do
  end ← isEOF
  unless end $ do
    readCase 1
