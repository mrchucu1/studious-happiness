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
cabeza (a:_) = a 

-- | Función que regresa la cola de una lista.
cola :: [a] -> [a]
cola (_:cx) = cx 

-- | Función que regresa el último elemento de una lista.
ultimo :: [a] -> a
ultimo []  = error "We don't do that here!"
ultimo [a] = a
ultimo a   = ultimo ( take 1 ( drop ((nolength a)-1) a ) )

-- | Función que devuelve la lista menos el último elemento.
casiTodos :: [a] -> [a]
casiTodos a = take ((nolength a) -1 ) a

-- | Función que regresa el n-ésimo elemento de atrás para adelante.
(!!!) :: [a] -> Int -> a
(!!!) a n = cabeza ( take 1 ( drop ((nolength a) - n) a ) )

-- | Función que nos dice si un elemento está en una lista.
existe :: (Eq a) => [a] -> a -> Bool
existe x q = nolength ( filter (== q) x )  /= 0

-- | Función que suma todos los elementos de una lista de números.
sumaNumsList :: [Int] -> Int
sumaNumsList x@(a:cx)
  | nolength x == 0 = 0
  | nolength x == 1 = a
  | nolength x > 1  = a + (sumaNumsList cx)

-- | Función que quita los repetidos de una lista.
repeticiones :: Eq a => [a] -> [a]
repeticiones [] = []
repeticiones (x:xs) = if existe xs x
    then repeticiones xs
    else x:repeticiones xs

-- | Función que voltea una lista.
-- Bad implementation, but it it what it is. TODO: it was not.
reversa :: [a] -> [a]
reversa [a]     = [a]
reversa x@(_:_) = [(ultimo x)] ++ (reversa (casiTodos x))

-- | Función que regresa una tupla (a, b) donde a es el elemento de la lista y b
--           es el número de veces que se repite 'a'.
cuantasVeces :: (Eq a) => [a] -> [(a, Int)]
cuantasVeces []     = []
cuantasVeces (x:xs) = (x, (count (x:xs) x)):cuantasVeces (remove xs x)

count :: (Eq a) => [a] -> a -> Int
count [] _     = 0
count (x:xs) e = if e == x then 1 + count xs e else count xs e

remove :: (Eq a) => [a] -> a -> [a]
remove [] _     = []
remove (x:xs) e = if e == x then remove xs e else x:(remove xs e)

--------------------------------------------------------------------------------
--------                     LISTAS POR COMPRENSIÓN                     --------
--------------------------------------------------------------------------------

-- | lista que contiene a los números impares de 0 a n.
impares n = [x | x <- [0..n], (x `mod` 2) /= 0]

-- | lista que contiene a los números de 0 a n, los cuales son múltiplos de k.
multiplosNK n k = [x | x <- [0..n], (x `mod` k) == 0]

-- | lista que contiene la suma de gauss de cada número desde 0 hasta n.
sumaDeGauss n = [sumaGauss x | x <- [0..n]]

-- | lista que contiene el producto cruz de dos listas.
productoCruz l1 l2 = [ (i,j) | i <- l1 , j <- l2 ]

-- | lista que regresa la diferencia simétrica de dos listas.
diferenciaSimetrica l1 l2 = [ x | x <- l1 ++ l2, ( ((existe l1 x) && not ( existe l2 x)) || ( (existe l2 x) && not ( existe l1 x) ) )]

--------------------------------------------------------------------------------
--------                            AUXILIARES                          --------
--------------------------------------------------------------------------------

sumaGauss :: Int -> Int
sumaGauss a = ((a+1)*a) `div` 2

nolength :: [a] -> Int
nolength []     = 0
nolength [a]    = 1
nolength (_:cx) = 1 + nolength cx

nodrop :: Int -> [a] -> [a]
nodrop _ [] = []
nodrop n xs@(_:xs')
    | n > 0     = nodrop (n-1) xs'
    | otherwise = xs


notake :: Int -> [a] -> [a]
notake 0 _ = []
notake n x@(a:cx)
    | (nolength x) <= n = x
    | otherwise         = if n <= 1 then [a] else [a] ++ notake (n-1) cx

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
