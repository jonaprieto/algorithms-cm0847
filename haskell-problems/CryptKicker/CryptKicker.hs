-- @Author: Jonathan Prieto-Cubides
-- | CryptKicker Problem

{-# LANGUAGE UnicodeSyntax #-}

module Main
    where

import           Control.Applicative ((<*>))
import           Control.Monad       (replicateM, unless)
import qualified Data.HashMap.Strict as M
import           Data.List           (delete, nub, intercalate)
import           Data.List.Split     (splitOn)
import           Data.Maybe          (fromJust, fromMaybe, isJust, isNothing)
import qualified Data.Set            as S
import           System.IO           (isEOF)

type Wrd    = String
type Ltr    = Char
type Map    = M.HashMap
type Set    = S.Set
type Dict   = Map Wrd Wrd
type Cipher = Map Char Char


removeWrd ∷ Map Wrd [Wrd] → Wrd → Map Wrd [Wrd]
removeWrd mapa d = M.map (delete d) mapa

removeOnlyWrd ∷ Map Wrd [Wrd] → Wrd → Wrd → Map Wrd [Wrd]
removeOnlyWrd mapa w d = M.insert w newVal mapa
  where
    old ∷ [Wrd]
    old = fromJust $ M.lookup w mapa

    newVal ∷ [Wrd]
    newVal = delete d old

addWrdDict ∷ Maybe Dict → Wrd → Wrd → Maybe Dict
addWrdDict dict w m = Just $ M.insert w m (fromJust dict)

decipher ∷  [Wrd] → Map Wrd [Wrd] → Maybe Dict → Maybe Cipher →  Maybe Dict
decipher  [] mapa dict cifra = dict
decipher l@(w:ws) mapa dict cifra
  | isNothing cifra = Nothing
  | isNothing dict  = Nothing
  | not (M.member w mapa) = Nothing
  | M.member w (fromJust dict) = decipher ws mapa dict cifra
  | null alternativas =  Nothing
  | not (validMatch w m (fromJust cifra)) = nextChoice
  | isJust nextLevel = nextLevel
  | otherwise =  nextChoice
  where
    alternativas ∷ [Wrd]
    alternativas = fromJust $ M.lookup w mapa

    m ∷ Wrd
    m  = head alternativas

    nextLevel ∷ Maybe Dict
    nextLevel = decipher ws (removeWrd mapa m) (addWrdDict dict w m) (updateCipher (fromJust cifra) w m )

    nextChoice ∷ Maybe Dict
    nextChoice = decipher l (removeOnlyWrd mapa w m) dict cifra

-- applyCipher ∷ Cipher → Wrd → Wrd
-- applyCipher cipher wrd = map (\c  → M.lookupDefault c c cipher) wrd

updateCipher ∷ Cipher → Wrd → Wrd → Maybe Cipher
updateCipher cipher [] []   = Just cipher
updateCipher _ [] _         = Nothing
updateCipher cipher p@(w:ws) m@(r:rs)
  | breakCipher cipher p m  = Nothing
  | otherwise               = updateCipher updated ws rs
  where
    updated ∷ Cipher
    updated = M.insert w r cipher

breakCipher ∷ Cipher → Wrd → Wrd → Bool
breakCipher _ [] _ = True
breakCipher cipher (w:ws) (r:rs)
  | replacement /= r   = False
  | otherwise          = breakCipher cipher ws rs
  where
    replacement ∷ Char
    replacement = (M.lookupDefault r w cipher)


validMatch ∷ Wrd → Wrd → Cipher → Bool
validMatch w r cipher
  | length w /= length r                    = False
  | go == nub go && from == nub from        = True
  | otherwise                               = not $ breakCipher cipher w r
  where
    simplied ∷ [(Ltr, Ltr)]
    simplied = nub $ zip w r

    go ∷ [Ltr]
    go = map fst simplied

    from ∷ [Ltr]
    from = map snd simplied

showLns ∷ [Wrd] → IO ()
showLns = putStrLn . (intercalate " ")

solveCase ∷ [Wrd] → IO ()
solveCase dict' = do
  end   ← isEOF
  unless end $ do
    ls  ← getLine
    let ws = splitOn " " ls
    let cifra = Just  M.empty
    let dict = Just M.empty

    let xs = M.fromListWith (++) ( map (\p → (length p, [p])) dict')
    let mapa = M.fromList $ map (\p → (p, fromMaybe [] (M.lookup (length p) xs))) ws
    case decipher ws mapa dict cifra of
      Just dd → showLns $ map (\p → fromJust $ M.lookup p dd) ws
      _ → showLns $ map (\p → concat (replicate (length p) "*")) ws
    solveCase dict'

main ∷ IO ()
main = do
  strN  ← getLine
  let n = read strN ∷ Int
  dict'  ← replicateM n (const getLine 1)
  end   ← isEOF
  solveCase dict'
