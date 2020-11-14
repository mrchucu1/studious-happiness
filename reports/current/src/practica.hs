tructuras Discretas 2021-1
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

module Bools where

import Compuertas

data MyBool = BTrue | BFalse deriving (Show, Eq, Ord)

instance Compuerta MyBool where

  -- | Método que representa la identidad para el tipo MyBool
  (.!.) BTrue = BTrue
  (.!.) BFalse = BFalse

  -- | Método que representa la compuerta neg para el tipo MyBool.
  (.¬.) = error "D:"

  -- | Método que representa la compuerta and para el tipo MyBool.
  (.^.) = error "D:"

  -- | Método que representa la compuerta or para el tipo MyBool.
  (.|.) = error "D:"

  -- | Método que representa la compuerta xor para el tipo MyBool.
  p .+. q = error "D:"

  -- | Método que representa la compuerta nand para el tipo MyBool.
  p .*. q = error "D:"

  -- | Método que representa la compuerta nor para el tipo MyBool.
  p .~. q = error "D:"

-- | Operador que representa la implicación.
(.->.) :: MyBool -> MyBool -> MyBool --ex
(.->.) = error "D:"

-- | Operador que representa la doble implicación.
(.<->.) :: MyBool -> MyBool -> MyBool --ex
(.<->.) = error "D:"
