-- @Author: jonaprieto
-- @Date:   2016-07-24 14:19:52
-- @Last Modified by:   jonaprieto
-- @Last Modified time: 2016-07-24 14:37:34
module ClosestPair where

import           Data.List

data Pto = Pto Int Int
    deriving (Eq)

instance Show Pto where
    show (Pto x y) = show (x,y)

data Edge = Edge Pto Pto
    deriving (Eq)

instance Show Edge where
    show (Edge x y) = show (x,y)


-- Closest Pair Algorithm:
-- Input: A set of point in the plane.
-- Output: A cyclic path that visit each point.

closest :: [Pto] -> [Edge]
closest xs = nub $ closest' xs (length xs) []


closest' :: [Pto] -> Int ->[Edge] -> [Edge]
closest' _ 0 used = used
closest' [] _  _   = []
closest' [x] _ _   = []
closest' [x,y] _ _ = [Edge x y]
closest' xs n used = newEdge ++ closest' xs (n-1) (newEdge ++ used)
    where
        newEdge :: [Edge]
        newEdge = findEdge xs dmin used
        dmin :: Double
        dmin = minDist xs

findEdge :: [Pto] -> Double -> [Edge] -> [Edge]
findEdge [] _ _     = []
findEdge [x] _ _    = []
findEdge (x:xs) d usedEdges
    | not (null edges)      = edges
    | otherwise             = findEdge xs d usedEdges
    where
        edges :: [Edge]
        edges = findEdge' x xs d usedEdges

findEdge' :: Pto -> [Pto] -> Double -> [Edge] -> [Edge]
findEdge' _ [] _ _ = []
findEdge' x (y:xs) d usedEdges
    | dist x y == d && not (xy `elem` usedEdges && xy `elem` usedEdges) = [xy]
    | otherwise = findEdge' x xs d usedEdges
    where
        xy = Edge x y

minDist :: [Pto] -> Double
minDist []      = read "Infinity"::Double
minDist [x]     = read "Infinity"::Double
minDist (x:xs)  = minimum [dX, dXS]
    where
        dX, dXS :: Double
        dX  = minimum $ map (dist x) xs
        dXS = minDist xs

-- The distance function in the plane
dist :: Pto -> Pto -> Double
dist (Pto x1 y1) (Pto x2 y2) = sqrt $ x + y
    where
        x = fromIntegral (x1-x2)^2
        y = fromIntegral (y1-y2)^2

x = [Pto 0 0, Pto 1 0, Pto 2 0, Pto 2 1, Pto 1 1, Pto 0 1]
