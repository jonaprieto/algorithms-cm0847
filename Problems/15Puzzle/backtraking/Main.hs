
-- | 15-Puzzle Game

{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Main where

import           Control.Lens  (element, (&), (.~))
import           Control.Monad
import           Data.List     (elemIndex, intercalate)
import           Data.Maybe    (fromJust)

debug = True

type Board    = [[Int]]
type Mov      = Char
type Position = (Int, Int)
type State    = (Board, [Mov])

printGame ∷ Board → IO ()
printGame [row1, row2, row3, row4] = do
  putStrLn $ intercalate "\t" $ map show row1
  putStrLn $ intercalate "\t" $ map show row2
  putStrLn $ intercalate "\t" $ map show row3
  putStrLn $ intercalate "\t" $ map show row4
  putStrLn "-"

validPosition ∷ Position → Bool
validPosition (x,y) = and [0 <= x, x <= 3, 0 <= y, y <= 3]

getNewPosition ∷ Position → Mov → Position
getNewPosition (x,y) 'L' = (x,y-1)
getNewPosition (x,y) 'R' = (x,y+1)
getNewPosition (x,y) 'U' = (x-1,y)
getNewPosition (x,y) 'D' = (x+1,y)

validMov ∷ Board → Mov → Mov →  Bool
validMov board mov last
  |  mov == 'L' && last /= 'R' = validPosition $ getNewPosition zero mov
  |  mov == 'R' && last /= 'L' = validPosition $ getNewPosition zero mov
  |  mov == 'U' && last /= 'D' = validPosition $ getNewPosition zero mov
  |  mov == 'D' && last /= 'U' = validPosition $ getNewPosition zero mov
  |  otherwise                 = False
  where
    zero ∷ Position
    zero = findHole board

findHole ∷ Board → Position
findHole board =
  let extPos = fromJust $ elemIndex 0 $ concat board
  in (extPos `div` 4, extPos `mod` 4 )

checkBoard ∷ Board → Bool
checkBoard board = isSorted . init $ concat board
  where
    isSorted ∷ [Int] → Bool
    isSorted xs = and $ zipWith (<=) xs (drop 1 xs)

(.@) ∷ Board → Position → Int
board .@ (x,y) = (board !! x) !! y

(.^) ∷ Board → (Position, Int) → Board
board .^ ((x,y), val) = board & element x . element y .~ val

makeMov ∷ Board → Mov → Board --the movement we aim is valid
makeMov board mov  = do
  let oldPosition ∷ Position -- zero position
      oldPosition = findHole board

  let newPosition ∷ Position
      newPosition = getNewPosition oldPosition mov

  let valTile = board .@ newPosition

  let firstReplacement ∷ Board
      firstReplacement = board .^ (newPosition, 0)

  let finalBoard ∷ Board
      finalBoard = firstReplacement .^ (oldPosition, valTile)

  finalBoard

toLeft ∷ Board → Board
toLeft board = makeMov board 'L'

toRight ∷ Board → Board
toRight board = makeMov board 'R'

toUp ∷ Board → Board
toUp board = makeMov board 'U'

toDown ∷ Board → Board
toDown board = makeMov board 'D'

-- | backtracking

orderMoves = "LRUD"


dfs ∷  State → Int → Int → IO State
dfs state 0 _  = return state
dfs state _ 4  = return state
dfs state@(board, last:movs) nIter nextMov = do
  when debug $ printGame board
  when debug $ putStrLn $ "movs: " ++ reverse (last:movs)

  let move ∷ Mov
      move = orderMoves!!nextMov

  when debug $ putStrLn $ "trying with: " ++  show move

  if | checkBoard board → return state
     | validMov board move last → do
        (newBoard, newMovs) ← dfs (makeMov board move, move:last:movs) (nIter-1) 0
        if | checkBoard newBoard → return (newBoard, newMovs)
           | otherwise → dfs state nIter (nextMov+1)
     | nextMov < 3 → dfs state nIter (nextMov+1)
     | otherwise   → return state

solveGame ∷ Board →  IO State
solveGame board  = dfs (board,['→']) 50 0

readRow ∷ IO [Int]
readRow = map read . words <$> getLine

readGame ∷ IO ()
readGame = do
  board ∷ Board ← replicateM 4 readRow
  when debug $ printGame board
  if | checkBoard board → putStrLn ""
     | otherwise → do
        (lastBoard, solution) ← solveGame board
        when debug $ putStrLn "lastBoard: "
        when debug $ printGame lastBoard
        when debug $ putStrLn "solution: "
        when debug $ putStrLn solution
        if | checkBoard lastBoard && findHole lastBoard == (3,3)
              → putStrLn $ tail $ reverse solution
           | otherwise  → putStrLn "This puzzle is not solvable."

main ∷ IO ()
main = do
  t ∷ Int ← read <$> getLine
  replicateM_ t readGame
