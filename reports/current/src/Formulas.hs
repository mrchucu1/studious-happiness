{-
- Estructuras Discretas 2021-1
- Profesor: Alma Rosario Arévalo Loyola
- Ayudante: José Ricardo Desales Santos
- Ayudante: Karla Socorro García Alcántara
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: Ing. Rodrigo Guadalupe Chávez Jiménez
- Practica 4: Interpretaciones de la lógica.
- Integrantes:
-
-
-}

module Formulas where

import Bools

data Prop =
  Var String
  | Neg Prop
  | Conj Prop Prop
  | Disy Prop Prop
  | Impl Prop Prop
  | Syss Prop Prop deriving(Show)

-- Todas las variables que se encuentren aquí, supondremos que son
-- verdaderas.
type Modelo = [String]

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

-- | niegalo. Función que niega una proposición.
niegalo :: Prop -> Prop
niegalo (Var x) = Neg (Var x)
niegalo (Neg p) = p
niegalo (Conj p q) = Disy (niegalo p) (niegalo q) 
niegalo (Disy p q) = Conj (niegalo p) (niegalo q) 
niegalo impl = niegalo (quitaImps impl)

-- | quitaImps. Función que quita las implicaciones de una
-- proposición, regresando una expresión equivalente.
quitaImps :: Prop -> Prop
quitaImps (Var x) = Var x
quitaImps (Neg p) = Neg (quitaImps p)
quitaImps (Conj p q) = Conj (quitaImps p) (quitaImps q)
quitaImps (Disy p q) = Disy (quitaImps p) (quitaImps q) 
quitaImps (Impl p q) = Disy (Neg (quitaImps p)) (quitaImps q) 
quitaImps (Syss p q) = Conj (Disy (Neg (quitaImps p)) (quitaImps q)) (Disy (Neg (quitaImps q)) (quitaImps p))

-- | interpreta. Función que interpreta una proposición dados sus
-- valores de verdad.
interpreta :: Modelo -> Prop -> MyBool
interpreta = error "D:"
{--
 --	B: Es un bebé.
 --	M: Puede manejar un cocodrilo.
 --	L: Es lógico.
 -- 	D: Es despistado.
 --
 --}

-- | e1. Función que representa el enunciado: Todos los bebé son ilógicos.
e1 :: Prop
e1 = error "D:"

-- | e2. Función que representa el enunciado: Nadie que sea despistado puede manejar un cocodrilo.
e2 :: Prop
e2 = error "D:"

-- | e3. Función que representa el enunciado: Las personas ilógicas son despistadas.
e3 :: Prop
e3 = error "D:"

-- | e. Función que representa el enunciado: e1 /\ e2 /\ e3 -> (B -> ¬M)
e :: Prop
e = error "D:"

-- | modelo. Función que representa el modelo con el que e es satisfacible.
modelo :: Modelo
modelo = error "D:"

--------------------------------------------------------------------------------
--------                             EJEMPLOS                           --------
--------------------------------------------------------------------------------

niegalo1 = niegalo (Var "P")
-- Regresa: Neg (Var "P")

niegalo2 = niegalo (Impl (Var "P") (Var "Q"))
-- Regresa: Conj (Var "P") (Neg (Var "Q"))

quitaImps1 = quitaImps (Conj (Impl (Var "P") (Var "Q")) (Var "P"))
-- Regresa: Conj (Disy (Neg (Var "P")) (Var "Q")) (Var "P")

quitaImps2 = quitaImps (Syss (Var "P") (Var "Q"))
-- Regresa: Conj (Disy (Neg (Var "P")) (Var "Q")) (Disy (Neg (Var "Q")) (Var "P"))

interpreta1 = interpreta [] (Disy (Var "P") (Neg (Var "P")))
-- Regresa: #t

interpreta2 = interpreta ["Q"] (Conj (Var "Q") (Neg (Var "Q")))
-- Regresa: #f

