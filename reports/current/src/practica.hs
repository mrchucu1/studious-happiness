{-
- Estructuras discretas 2021-1
- Profesor: Alma Rosario Arévalo Loyola
- Ayudante: José Ricardo Desales Santos
- Ayudante: Karla Socorro García Alcántara
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: Rodrigo Guadalupe Chávez Jiménez
- Practica 1: Introducción a Haskell y funciones.
- Integrantes: Diego Navarro,
-
-
-}

module Funciones where

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

-- | Función que regresa el sucesor de un número, esto es el número más uno.
sucN :: Int -> Int
sucN x = x + 1

-- | Función que regresa el máximo de dos números.
maxNum :: Int -> Int -> Int
maxNum a b = if a >= b
             then a else b

-- | Función que suma dos números.
sumaNum :: Int -> Int -> Int
sumaNum a b = a + b

-- | Función que resta dos números (El primero menos el segundo).
restaNum :: Int -> Int -> Int
restaNum a b = a - b

-- | Función que multiplica dos números.
multNum :: Int -> Int -> Int
multNum a b = a * b

-- | Función que divide dos números (El primreo es el numerador).
divNum :: Int -> Int -> Int
divNum n d = if d == 0
             then 0
             else n `div` d

-- | Función que regresa la negación de una proposición.
--   Por ejemplo: True y False
negP :: Bool -> Bool
negP a = not a

-- | Función que regresa la conjunción de dos proposiciones.
--   Por ejemplo: True y False
conjP :: Bool -> Bool -> Bool
conjP a b = a && b

-- | Función que regresa la disyunción de dos proposiciones.
--   Por ejemplo: True y False
disyP :: Bool -> Bool -> Bool
disyP a b =  a || b

-- | Función que calcula el valor absoluto de un número.
absNum :: Int -> Int
absNum a = abs a

-- | Función que regresa el área de un círculo.
areaCirc :: Double -> Double
areaCirc r = pi * r**2

-- | Función que regresa la distancia entre dos puntos (x1, x2), (y1. y2).
distancia :: Double -> Double -> Double -> Double -> Double
distancia x1 x2 y1 y2 = ( (x1 - y1)**2 + (x2 - y2)**2 ) ** (1/2)

-- | Función que calcula la suma de los primeros n números (Suma de Gauss).
sumaGauss :: Int -> Int
sumaGauss a = (a-1)*a

-- | Función que calcula el área de un triángulo dados tres puntos.
areaTri :: Double -> Double -> Double -> Double -> Double -> Double -> Double
areaTri x1 y1 x2 y2 x3 y3 = abs ((x1*y2)+(x2*y3)+(x3*y1)-(y1*x2)-(y2*x3)-(y3*x1)) * (1/2)

--------------------------------------------------------------------------------
--------                             PRUEBAS                            --------
--------------------------------------------------------------------------------
pruebaEjemplo = sucN 5
--assert _ pruebaEjemplo = 6
-- Regresa: 6

maxNum1 = maxNum 1 0
--assert _ maxNum1 = 1
-- Regresa: 1

maxNum2 = maxNum (-1) 0
--assert _ maxNum2 = 0
-- Regresa: 0

sumaNum1 = sumaNum 1 3
--assert _ sumaNum1 = 4
-- Regresa: 4

sumaNum2 = sumaNum (-7) 8
--assert _ sumaNum2 = 1
-- Regresa: 1

restaNum1 = restaNum 9 6
--assert _ restaNum1 = 3
-- Regresa: 3

restaNum2 = restaNum 1 3
--assert _ restaNum2 = -2
-- Regresa: -2

restaNum3 = restaNum (-1) 1
--assert _ restaNum3 = -2
-- Regresa: -2

multNum1 = multNum 0 3
--assert _ multNum1 = 0
-- Regresa: 0

multNum2 = multNum 9 8
--assert _ multNum2 = 72
-- Regresa: 72

divNum1 = divNum 4 2
--assert _ divNum1 = 2
-- Regresa: 2

divNum2 = divNum 9 4
--assert _ divNum2 = 2
-- Regresa: 2

negP1 = negP True
--assert _ negP1 = False
-- Regresa: False

negP2 = negP False
--assert _ negP2 = False
-- Regresa: True

conjP1 = conjP True True
--assert _ conjP1 = True
-- Regresa: True

conjP2 = conjP False True
--assert _ conjP2 = False
-- Regresa: False

disyP1 = disyP False False
--assert _ disyP1 = False
-- Regresa: False

disyP2 = disyP True False
--assert _ disyP2 = True
-- Regresa: True

absNum1 = absNum 9
--assert _ absNum1 = 9
-- Regresa: 9

absNum2 = absNum (-9)
--assert _ absNum2 = 9
-- Regresa: 9

areaCirc1 = areaCirc 2
--assert _ areaCirc1 = 12.566370614359172
-- Regresa: 12.57

areaCirc2 = areaCirc 2.5
--assert _ areaCirc2 = 19.634954084936208
-- Regresa: 19.63

distancia1 = distancia 1 2 (-3) 4
--assert _ distancia1 = 4.47
-- Regresa: 4.47

distancia2 = distancia (-3) 0 (-4) 6
--assert _ distancia2 = 6.08
-- Regresa: 6.08

sumaGauss1 = sumaGauss 10
--assert _ sumaGauss1 = 55
-- Regresa: 55

sumaGauss2 = sumaGauss 1000
--assert _ sumaGauss2 = 500500
-- Regresa: 500500

areaTri1 = areaTri (-8) (-2) 4 6 1 5
--assert _ areaTri1 = 6.0
-- Regresa: 6.0

areaTri2 = areaTri (-8) (-2) 4 6 (-1) (-5)
--assert _ areaTri2 = 46.0
-- Regresa: -46.0
