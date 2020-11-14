{-
- Estructuras Discretas 2021-1
- Profesor: Alma Rosario Arévalo Loyola
- Ayudante: José Ricardo Desales Santos
- Ayudante: Karla Socorro García Alcántara
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: Ing. Rodrigo Guadalupe Chávez Jiménez
- Ejercicio Semanal 2: Nuestros Booleanos
- Integrantes:
-
-
-}

module Bools
  ((.^.),
   (.|.),
   (.¬.),
   (.->.),
   (.<->.),
   transform,
   MyBool (..)
  ) where

import Compuertas

data MyBool = BTrue | BFalse deriving (Eq, Ord)
instance Show MyBool where
    show BTrue  = "#t"
    show BFalse = "#f"

instance Compuerta MyBool where

  -- | Método que representa la identidad para el tipo MyBool
  (.!.) BTrue = BTrue
  (.!.) BFalse = BFalse

  -- | Método que representa la compuerta neg para el tipo MyBool.
  (.¬.) BTrue = BFalse
  (.¬.) BFalse = BTrue

  -- | Método que representa la compuerta and para el tipo MyBool.
  (.^.) BTrue BTrue = BTrue
  (.^.) _ _ = BFalse

  -- | Método que representa la compuerta or para el tipo MyBool.
  (.|.) BTrue _ = BTrue
  (.|.) _ BTrue = BTrue
  (.|.) _ _ = BFalse

  -- | Método que representa la compuerta xor para el tipo MyBool.
  (.+.) BTrue BFalse = BTrue
  (.+.) BFalse BTrue = BTrue
  (.+.) _ _ = BFalse

  -- | Método que representa la compuerta nand para el tipo MyBool.
  (.*.) a b = (.¬.) ((.^.) a b)

  -- | Método que representa la compuerta nor para el tipo MyBool.
  (.~.) a b = (.¬.) ((.|.) a b)

-- | Operador que representa la implicación.
(.->.) :: MyBool -> MyBool -> MyBool --ex
(.->.) BFalse _= BTrue
(.->.) BTrue BFalse = BFalse
(.->.) BTrue BTrue = BTrue

-- | Operador que representa la doble implicación.
(.<->.) :: MyBool -> MyBool -> MyBool --ex
(.<->.) BTrue BTrue = BTrue
(.<->.) BFalse BFalse = BTrue
(.<->.) _ _ = BFalse

-- | transform. Función que transforma el tipo Bool de haskell a
-- nuestros booleanos.
transform :: Bool -> MyBool
transform True  = BTrue
transform False = BFalse
