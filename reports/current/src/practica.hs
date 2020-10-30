{-
- Estructuras Discretas 2021-1
- Profesor: Alma Rosario Arévalo Loyola
- Ayudante: José Ricardo Desales Santos
- Ayudante: Karla Socorro García Alcántara
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: Ing. Rodrigo Guadalupe Chávez Jiménez
- Practica 3: Funciones sobre listas
- Integrantes: Diego Navarro Macias
-
-
-}

module FuncionesListas where

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

-- | Función que regresa la cabeza de una lista.
cabeza :: [a] -> a
cabeza (a:cx) = a 

-- | Función que regresa la cola de una lista.
cola :: [a] -> [a]
cola (a:cx) = cx 

-- | Función que regresa el último elemento de una lista.
ultimo :: [a] -> a
ultimo [a] = a
ultimo (_:a) = ultimo a 
ultimo [] = error "You have no power here!"

-- | Función que devuelve la lista menos el último elemento.
casiTodos :: [a] -> [a]
casiTodos a = take ((length a) - 1 ) a

-- | Función que regresa el n-ésimo elemento de atrás para adelante.
(!!!) :: [a] -> Int -> a
(!!!) a n = cabeza ( take 1 (drop n a ) )

-- | Función que nos dice si un elemento está en una lista.
existe :: (Eq a) => [a] -> a -> Bool
existe x q = length ( filter (== q) x )  /= 0

-- | Función que suma todos los elementos de una lista de números.
sumaNumsList :: [Int] -> Int
sumaNumsList x@(a:cx)
  | length x == 0 = 0
  | length x == 1 = a
  | length x > 1  = a + (sumaNumsList cx)

-- | Función que quita los repetidos de una lista.
repeticiones :: Eq a => [a] -> [a]
repeticiones x@(a:b:c:cx)
  | x == []       = []
  | length x == 1 = [a]
  | length x == 2 = if a == b then [a] else [a,b]
  | length x == 3 = if a == b && a == c then [a] else if b == c then [a,b] else [a,b,c]
  | otherwise     = if a == b then [a] ++ (repeticiones ( [a] ++ cx ) ) else [a,b] ++ (repeticiones cx)

-- | Función que voltea una lista.
reversa :: [a] -> [a]
reversa = error "D:"

-- | Función que regresa una tupla (a, b) donde a es el elemento de la lista y b
--           es el número de veces que se repite 'a'.
cuantasVeces :: (Eq a) => [a] -> a -> [(a, Int)]
cuantasVeces = error "D:"

--------------------------------------------------------------------------------
--------                     LISTAS POR COMPRENSIÓN                     --------
--------------------------------------------------------------------------------

-- | lista que contiene a los números impares de 0 a n.
impares n = error "D:"

-- | lista que contiene a los números de 0 a n, los cuales son múltiplos de k.
multiplosNK n k = error "D:"

-- | lista que contiene la suma de gauss de cada número desde 0 hasta n.
sumaDeGauss n = error ":O"

-- | lista que contiene el producto cruz de dos listas.
productoCruz l1 l2 = error "D:"

-- | lista que regresa la diferencia simétrica de dos listas.
diferenciaSimetrica l1 l2 = error "D:"

--------------------------------------------------------------------------------
--------                            AUXILIARES                          --------
--------------------------------------------------------------------------------

sumaGauss :: Int -> Int
sumaGauss a = ((a+1)*a) `div` 2

--------------------------------------------------------------------------------
--------                             EJEMPLOS                           --------
--------------------------------------------------------------------------------

lnums = [1,2,3,4]

lchar = ['a', 'b', 'c', 'd']

limprs = [1,3,5,7,9]

lmults = [0,3,6,9]

lgauss = [0,1,3,6,10,15,21,28,36,45,55]

lpcruz = [(1,0),(1,3),(1,6),(1,9),(2,0),(2,3),(2,6),(2,9),(3,0),(3,3),(3,6),(3,9),(4,0),(4,3),(4,6),(4,9)]

ldiffs = [0,5,6,7,8,9,10]

cabeza1 = cabeza lnums
-- Regresa: 1

cabeza2 = cabeza lchar
-- Regresa: 'a'

cola1 = cola lnums
-- Regresa: [2,3,4]

cola2 = cola lchar
-- Regresa: ['b','c','d']

ultimo1 = ultimo lnums
-- Regresa: 4

ultimo2 = ultimo lchar
-- Regresa: 'd'

casiTodos1 = casiTodos lnums
-- Regresa: [1,2,3]

casiTodos2 = casiTodos lchar
-- Regresa: ['a','b','c']

op1 = (!!!) lnums 0
-- Regresa: 4

op2 = (!!!) lchar 3
-- Regresa: 'a'

existe1 = existe lnums 1
-- Regresa: True

existe2 = existe lchar 'e'
-- Regresa: False

sumaNumsList1 = sumaNumsList lnums
-- Regresa: 10

sumaNumsList2 = sumaNumsList $ lnums ++ lnums ++ lnums
-- Regresa: 30

repeticiones1 = repeticiones [1,1,1,1,2,2,2,3,3,3,3,4,4,4,4,5,6,7,7,1]
-- Regresa: [1,2,3,4,5,6,7] EL ORDEN PUEDE VARIAR; solo importan los elementos.

repeticiones2 = repeticiones [(1,2), (2,1), (0,1), (1,2), (1,2), (0,1), (1,2), (2,1), (3,4), (1,2)]
-- Regresa: [(0,1),(1,2),(2,1),(3,4)] EL ORDEN PUEDE VARIAR; solo importan los elementos.

reversa1 = reversa lnums
-- Regresa: [4,3,2,1]

reversa2 = reversa lchar
-- Regresa: ['d','c','b','a']

imparesP = impares 10 == limprs
-- Regresa: True

multipsP = multiplosNK 10 3 == lmults
-- Regresa: True

gaussssP = sumaDeGauss 10 == lgauss
-- Regresa: True

procruzP = productoCruz lnums lmults == lpcruz
-- Regresa: True

difffffP = diferenciaSimetrica [0..10] lnums == ldiffs
-- Regresa: True
