
-- | Triqui Game

{-# LANGUAGE UnicodeSyntax #-}

module Triqui
    where

import           Data.List  (intercalate, nub)
import           Data.Maybe (catMaybes)

data Ficha = Vacia
           | Circulo
           | Cruz
           deriving Eq

instance Show Ficha where
  show Vacia   = " "
  show Circulo = "O"
  show Cruz    = "X"


data Fila = Fila Ficha Ficha Ficha deriving Eq

instance Show Fila where
    show (Fila c1 c2 c3) = show c1 ++ " | " ++ show c2 ++ " | " ++ show c3

data Tablero = Tablero Fila Fila Fila deriving Eq

instance Show Tablero where
  show t =
    intercalate "\n"
      [ intercalate " | "
          [if  t!!!n == Vacia then show n else show (t!!!n) |  n ← [1..3] ]
      , replicate 9 '-'
      , intercalate " | "
          [if  t!!!n == Vacia then show n else show (t!!!n) |  n ← [4..6] ]
      , replicate 9 '-'
      , intercalate " | "
          [if  t!!!n == Vacia then show n else show (t!!!n) |  n ← [7..9] ]
      ]

type Tableros = [Tablero]
type Jugador = Ficha

-- Dado un tablero muestra las diferentes estrategias que permite
-- hacer un buen juego: se define buen juego como ganar o empatar
resolver ∷ Tablero → Tableros
resolver = undefined

obtenerTablero ∷ IO Tablero
obtenerTablero = undefined

-- tablero:
-- 1 2 3
-- 4 5 6
-- 7 8 9

(!!!) ∷ Tablero → Int → Ficha
(Tablero (Fila x _ _ ) _ _ ) !!! 1 = x
(Tablero (Fila _ x _ ) _ _ ) !!! 2 = x
(Tablero (Fila _ _ x ) _ _ ) !!! 3 = x
(Tablero _ (Fila x _ _ ) _ ) !!! 4 = x
(Tablero _ (Fila _ x _ ) _ ) !!! 5 = x
(Tablero _ (Fila _ _ x ) _ ) !!! 6 = x
(Tablero _ _ (Fila x _ _ ) ) !!! 7 = x
(Tablero _ _ (Fila _ x _ ) ) !!! 8 = x
(Tablero _ _ (Fila _ _ x ) ) !!! 9 = x
-- t !!! 2

-- extraer una columna: t .! col
(.!) ∷ Tablero → Int → [Ficha]
t .! col = [ t !!! (3*(fila-1) + col) | fila ← [1..3] ]

-- extraer una fila: t .@ fila
(.@) ∷ Tablero → Int → [Ficha]
t .@ 1 = [ t !!! i | i ← [1,2,3] ]
t .@ 2 = [ t !!! i | i ← [4,5,6] ]
t .@ 3 = [ t !!! i | i ← [7,8,9] ]

diagonal1 ∷ Tablero → [Ficha]
diagonal1 t = [t!!! 1, t!!!5, t!!!9]

diagonal2 ∷ Tablero → [Ficha]
diagonal2 t = [t!!! 3, t!!!5, t!!!7]

esquina1 ∷ Tablero → [Ficha]
esquina1 t = [t!!! 1, t!!!2, t!!!4]

esquina2 ∷ Tablero → [Ficha]
esquina2 t = [t!!! 2, t!!!3, t!!!6]

esquina3 ∷ Tablero → [Ficha]
esquina3 t = [t!!! 4, t!!!7, t!!!8]

esquina4 ∷ Tablero → [Ficha]
esquina4 t = [t!!! 6, t!!!8, t!!!9]

contarFicha ∷ [Ficha] → Ficha → Int
contarFicha ls f = length $ filter (== f) ls

cambiarJugador ∷ Jugador → Jugador
cambiarJugador Cruz    = Circulo
cambiarJugador Circulo = Cruz
cambiarJugador x       = x


amenazaFila ∷ Tablero → Int → Jugador → Maybe Int
amenazaFila tablero fila actual
  | contarFicha contenido otro == 2  =
      case encontrarPosicion contenido 1 of
        Nothing  → Nothing
        Just col → Just $ 3*(fila-1) + col
  | otherwise = Nothing
  where
    contenido = tablero .@ fila
    otro = cambiarJugador actual

amenazaColumna ∷ Tablero → Int → Jugador → Maybe Int
amenazaColumna tablero col actual
  | contarFicha contenido otro == 2 =
      case encontrarPosicion contenido 1 of
        Nothing   → Nothing
        Just fila → Just (3*(fila-1) + col)
  | otherwise = Nothing
  where
    contenido = tablero .! col
    otro = cambiarJugador actual

amenazaDiagonal1 ∷ Tablero → Jugador → Maybe Int
amenazaDiagonal1 tablero actual
  | contarFicha contenido otro == 2 =
      case encontrarPosicion contenido 1 of
        Just 1  → Just 1
        Just 2  → Just 5
        Just 3  → Just 9
        Nothing → Nothing
  | otherwise = Nothing
  where
    contenido = diagonal1 tablero
    otro = cambiarJugador actual

amenazaDiagonal2 ∷ Tablero → Jugador → Maybe Int
amenazaDiagonal2 tablero actual
  | contarFicha contenido otro == 2 =
      case encontrarPosicion contenido 1 of
        Nothing → Nothing
        Just 1  → Just 3
        Just 2  → Just 5
        Just 3  → Just 7
  | otherwise = Nothing
  where
    contenido = diagonal2 tablero
    otro = cambiarJugador actual

amenazaEsquina1 ∷ Tablero → Jugador → Maybe Int
amenazaEsquina1 tablero actual
  | contarFicha contenido otro == 2 =
      case encontrarPosicion contenido 1 of
        Nothing → Nothing
        Just 1  → Just 1
        Just 2  → Just 2
        Just 3  → Just 4
  | otherwise = Nothing
  where
    contenido = esquina1 tablero
    otro = cambiarJugador actual

amenazaEsquina2 ∷ Tablero → Jugador → Maybe Int
amenazaEsquina2 tablero actual
  | contarFicha contenido otro == 2 =
      case encontrarPosicion contenido 1 of
        Nothing → Nothing
        Just 1  → Just 2
        Just 2  → Just 3
        Just 3  → Just 6
  | otherwise = Nothing
  where
    contenido = esquina2 tablero
    otro = cambiarJugador actual

amenazaEsquina3 ∷ Tablero → Jugador → Maybe Int
amenazaEsquina3 tablero actual
  | contarFicha contenido otro == 2 =
      case encontrarPosicion contenido 1 of
        Nothing → Nothing
        Just 1  → Just 4
        Just 2  → Just 7
        Just 3  → Just 8
  | otherwise = Nothing
  where
    contenido = esquina3 tablero
    otro = cambiarJugador actual

amenazaEsquina4 ∷ Tablero → Jugador → Maybe Int
amenazaEsquina4 tablero actual
  | contarFicha contenido otro == 2 =
      case encontrarPosicion contenido 1 of
        Nothing → Nothing
        Just 1  → Just 6
        Just 2  → Just 8
        Just 3  → Just 9
  | otherwise = Nothing
  where
    contenido = esquina4 tablero
    otro = cambiarJugador actual

encontrarPosicion ∷ [Ficha] → Int → Maybe Int
encontrarPosicion [] _ = Nothing
encontrarPosicion (f:fs) col
  | f == Vacia  = Just col
  | otherwise   = encontrarPosicion fs (col+1)

amenazas ∷ Tablero → Jugador →  [Int]
amenazas tablero player = catMaybes $
     [ amenazaFila tablero fila player    | fila  ← [1..3] ]
  ++ [ amenazaColumna tablero col player  | col   ← [1..3] ]
  ++ [ amenazaDiagonal1 tablero player ]
  ++ [ amenazaDiagonal2 tablero player ]
  ++ [ amenazaEsquina1 tablero player ]
  ++ [ amenazaEsquina2 tablero player ]
  ++ [ amenazaEsquina3 tablero player ]
  ++ [ amenazaEsquina4 tablero player ]

estaLleno ∷ Tablero → Bool
estaLleno t =
 contarFicha ( t .@ 1) Vacia == 0
 && contarFicha ( t .@ 2) Vacia == 0
 && contarFicha ( t .@ 3) Vacia == 0

amenaza ∷ Tablero → Jugador → Maybe Int
amenaza tabler player =
  case amenazas tabler player of
    []    → Nothing
    (x:_) → Just x

jugar ∷ Tablero → Int → Ficha → Tablero
jugar (Tablero (Fila Vacia y z) f2 f3) 1  f = Tablero (Fila f y z) f2 f3
jugar (Tablero (Fila x Vacia z) f2 f3) 2  f = Tablero (Fila x f z) f2 f3
jugar (Tablero (Fila x y Vacia) f2 f3) 3  f = Tablero (Fila x y f) f2 f3
jugar (Tablero f1 (Fila Vacia y z) f3) 4  f = Tablero f1 (Fila f y z) f3
jugar (Tablero f1 (Fila x Vacia z) f3) 5  f = Tablero f1 (Fila x f z) f3
jugar (Tablero f1 (Fila x y Vacia) f3) 6  f = Tablero f1 (Fila x y f) f3
jugar (Tablero f1 f2 (Fila Vacia y z)) 7  f = Tablero f1 f2 (Fila f y z)
jugar (Tablero f1 f2 (Fila x Vacia z)) 8  f = Tablero f1 f2 (Fila x f z)
jugar (Tablero f1 f2 (Fila x y Vacia)) 9  f = Tablero f1 f2 (Fila x y f)
jugar t _ _                                 = t

tableroInicial ∷ Tablero
tableroInicial =
  Tablero (Fila Vacia Vacia Vacia)
          (Fila Vacia Vacia Vacia)
          (Fila Vacia Vacia Vacia)

llenarVacio ∷ (Tablero, [Int]) → Jugador →  (Tablero, [Int])
llenarVacio (Tablero (Fila Circulo _ _ )
                     (Fila _ Cruz _ )
                     (Fila _ _ Circulo) , _) _ =
  (Tablero  (Fila Circulo Cruz Vacia)
            (Fila  Vacia Cruz Vacia)
            (Fila Vacia Vacia Circulo), [])
llenarVacio (Tablero  (Fila _ _ Circulo)
                      (Fila _ Cruz _ )
                      (Fila Circulo _ _) , _) _ =
  (Tablero  (Fila Vacia Cruz Circulo)
            (Fila  Vacia Cruz Vacia)
            (Fila Circulo Vacia Vacia), [])
llenarVacio (t, []) _ = (t, [])
llenarVacio (t, pos:ls ) p
  | nuevoTablero /= t = (nuevoTablero, ls)
  | otherwise         = llenarVacio (t, ls) p
  where
    nuevoTablero ∷ Tablero
    nuevoTablero = jugar t pos p

render :: Tablero -> IO ()
render tablero = do
  putStrLn $ replicate 100 '\n'
  print tablero
  putStrLn "\n"

renderGanador :: Jugador -> IO ()
renderGanador ganador =
  case ganador of
    Vacia   -> putStrLn "It was a draw"
    Circulo -> putStrLn "You Won"
    Cruz    -> putStrLn "Machine Won"

volverJugar ∷ (Tablero, Jugador) → IO (Tablero, Jugador)
volverJugar (tablero, Cruz) = do
  render tablero
  if estaLleno tablero
    then do
      putStrLn $ replicate 100 '\n'
      putStrLn "Draw!"
      return (tablero, Vacia)
    else
      case amenaza tablero Circulo of
        Nothing →
          case amenaza tablero Cruz of
            Nothing → do
              let estrategia = [5,1,3,7,9,4,6,2,8]
              let (nuevoTablero,_) = llenarVacio (tablero,estrategia) Cruz
              volverJugar (nuevoTablero, Circulo)
            Just positionCirculo → do
              let nuevoTablero = jugar tablero positionCirculo Cruz
              volverJugar (nuevoTablero, Circulo)
        Just position → do
          let nuevoTablero = jugar tablero position Cruz
          putStrLn $ replicate 100 '\n'
          return (nuevoTablero, Cruz)

volverJugar (tablero, Circulo) = do
  render tablero
  if estaLleno tablero
    then do
      putStrLn $ replicate 100 '\n'
      putStrLn "Draw!"
      return (tablero, Vacia)
    else do
      putStrLn "What is your move? choose a number 1..9"
      jugada ← getLine
      let intJugada = read jugada ∷ Int
      let listaAmenazas = amenazas tablero Cruz
      let nuevoTablero = jugar tablero intJugada Circulo
      if nuevoTablero == tablero
        then volverJugar (tablero, Circulo)
        else
          if intJugada `elem` listaAmenazas
            then do
              return (nuevoTablero, Circulo)
            else volverJugar (nuevoTablero, Cruz)

main ∷ IO ()
main = do
  putStrLn "Do you move first? [y/n]"
  player ← getLine
  if player == "N" || player == "n"
    then do
      (tablero,ganador) ← volverJugar (tableroInicial, Cruz)
      render tablero
      renderGanador ganador
    else do
      (tablero,ganador) ← volverJugar (tableroInicial, Circulo)
      render tablero
      renderGanador ganador
