-- | CryptKicker Problem

{-# LANGUAGE UnicodeSyntax #-}

module Main
    where

import           Control.Monad       (replicateM, unless)
import qualified Data.HashMap.Strict as M
import           Data.List           (delete, nub, sort)
import           Data.List.Split     (splitOn)
import           Data.Maybe          (fromJust, fromMaybe, isJust, isNothing)
import           System.IO           (isEOF)

type Wrd    = String
type Ltr    = Char
type Map    = M.HashMap
type Dict   = Map Wrd Wrd
type Cipher = Map Char Char


decipher ∷  [Wrd] → Map Wrd [Wrd] → Maybe Dict → Maybe Cipher →  Maybe Dict
decipher  [] _ dict _ = dict
decipher l@(w:ws) mapa dict cipher
  | noValid cipher                          = Nothing
  | noValid dict                            = Nothing
  | not $ M.member w mapa                   = Nothing
  | hasTranslation w dict                   = decipher ws mapa dict cipher
  | null choices                            = Nothing
  | not $ validMatch w m cipher'            = nextChoiceStep
  | isJust takeChoice                       = takeChoice
  | otherwise                               = nextChoiceStep
  where
    choices ∷ [Wrd]
    choices = sort $ fromJust $ M.lookup w mapa

    m ∷ Wrd
    m  = head choices

    takeChoice ∷ Maybe Dict
    takeChoice = decipher ws (removeWrd mapa m) (addWrdDict dict w m) (updateCipher cipher' w m )

    nextChoiceStep ∷ Maybe Dict
    nextChoiceStep = decipher l (removeOnlyWrd mapa w m) dict cipher

    cipher' ∷ Cipher
    cipher' = fromJust cipher

hasTranslation ∷ Wrd → Maybe Dict → Bool
hasTranslation w dict = case dict of
  Just d → M.member w d
  _      → False

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


updateCipher ∷ Cipher → Wrd → Wrd → Maybe Cipher
updateCipher cipher [] []   = Just cipher
updateCipher _ [] _         = Nothing
updateCipher _ _ []         = Nothing
updateCipher cipher p@(w:ws) m@(r:rs)
  | breakCipher cipher p m  = Nothing
  | otherwise               = updateCipher updated ws rs
  where
    updated ∷ Cipher
    updated = M.insert w r cipher

breakCipher ∷ Cipher → Wrd → Wrd → Bool
breakCipher _ [] _ = False
breakCipher _ _ [] = False
breakCipher cipher (w:ws) (r:rs)
  | M.member w cipher           = replacement /= r
  | r `elem` M.elems cipher     = True
  | otherwise                   = breakCipher cipher ws rs
  where
    replacement ∷ Char
    replacement = M.lookupDefault r w cipher


validMatch ∷ Wrd → Wrd → Cipher → Bool
validMatch w r cipher
  | length w /= length r               = False
  | go /= nub go || from /= nub from   = False
  | otherwise                          = not $ breakCipher cipher w r
  where
    simplied ∷ [(Ltr, Ltr)]
    simplied = nub $ zip w r

    go ∷ [Ltr]
    go = map fst simplied

    from ∷ [Ltr]
    from = map snd simplied

valid ∷ Maybe a → Bool
valid = isJust

noValid ∷ Maybe a → Bool
noValid = isNothing

showLns ∷ [Wrd] → IO ()
showLns = putStrLn . unwords

solveCase ∷ [Wrd] → IO ()
solveCase dictionary = do
  end   ← isEOF
  unless end $ do
    ls  ← getLine

    let wds ∷ [Wrd]
        wds = splitOn " " ls

    let cipher ∷ Maybe Cipher
        cipher = Just  M.empty

    let dict ∷ Maybe Dict
        dict = Just M.empty

    let xs ∷ Map Int [Wrd]
        xs = M.fromListWith (++) (map (\p → (length p, [p])) dictionary)

    let mapa ∷ Map Wrd [Wrd]
        mapa =
          M.fromList $ map (\p → (p, fromMaybe [] (M.lookup (length p) xs))) wds

    case decipher wds mapa dict cipher of
      Just dd → showLns $ map (\p → fromJust $ M.lookup p dd) wds
      _       → showLns $ map (\p → concat (replicate (length p) "*")) wds

    solveCase dictionary

main ∷ IO ()
main = do
  strN  ← getLine
  let n = read strN ∷ Int
  dict  ← replicateM n getLine
  solveCase dict
