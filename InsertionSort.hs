-- @Author: jonaprieto
-- @Date:   2016-07-24 13:59:08
-- @Last Modified by:   jonaprieto
-- @Last Modified time: 2016-07-24 14:17:42

module InsertionSort
    where

isort :: Ord a => [a] -> [a]
isort []      = []
isort (x:xs)  = put x $ isort xs

put :: Ord a => a -> [a] -> [a]
put x []     = [x]
put x (y:ys)
    | x <= y    = x:y:ys
    | otherwise = y:put x ys
