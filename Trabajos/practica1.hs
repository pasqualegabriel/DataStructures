-- Conceptos basicos:

-- 1 A
--Proposito: Dado un numero retorna el siguiente
--Precondicion: -
succ :: Int -> Int
succ x = x + 1

-- 1 B
--Proposito: Dados dos enteros, retorna la suma de ellos 
--Precondicion: -
sumar :: Int -> Int -> Int
sumar x y = x + y

-- 1 C
--Proposito: Dados dos numeros retorna el maximo
--Precondicion: Ninguna
maximo :: Int -> Int -> Int
maximo x y = if x > y
             then x
             else y

maximo2 :: Int -> Int -> Int
maximo2 x y 
       | x < y = y 
       | otherwise = x 

-- 2 A
--Proposito: Dado un bool 
--Precondicion: 
negar :: Bool -> Bool
negar True = False
negar False = True

-- 2 B
--Proposito: Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
--Precondicion:
andLogico :: Bool -> Bool -> Bool
andLogico True True = True
andLogico _ _ = False

-- 2 C
--Proposito: Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
--Precondicion: -
orLogico :: Bool -> Bool -> Bool
orLogico False False = False
orLogico _ _ = True

-- 2 D
--Proposito: Dado un par de numeros devuelve la primera componente.
--Precondicion: -
primera :: (Int,Int) -> Int
primera (x,y) = x

-- 2 E
--Proposito: Dado un par de numeros devuelve la segunda componente.
--Precondicion: 
segunda :: (Int,Int) -> Int
segunda (x,y) = y

-- 2 F
--Proposito: Dado un par de numeros devuelve su suma.
--Precondicion: -
sumaPar :: (Int,Int) -> Int
sumaPar (x,y) = x + y

-- 2 G
--Proposito: Dado un par de numeros devuelve el mayor de estos.
--Precondicion: -
maxDelPar :: (Int,Int) -> Int
maxDelPar (x,y) = maximo x y

-- 3 A
--Proposito: Dado un elemento de algun tipo devuelve ese mismo elemento.
--Precondicion: -
loMismo :: a -> a
loMismo x = x

-- 3 B
--Proposito: Dado un elemento de algun tipo devuelve el n ́umero 7.
--Precondicion: -
siempreSiete :: a -> Int
siempreSiete _ = 7

-- 3 C
--Proposito: Dado un elemento de algun tipo devuelve un par con ese elemento en ambas componentes.
--Precondicion: -
duplicar :: a -> (a,a)
duplicar x = (x,x)

-- 3 D
--Proposito: Dado un elemento de algun tipo devuelve una lista con este unico elemento.
--Precondicion: -
singleton :: a -> [a]
singleton x = [x]

-- 4 A
--Proposito: Dada una lista de elementos, si es vacia devuelve True, sino devuelve False.
--Precondicion: -
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty xs = False

-- 4 B
--Proposito: Dada una lista devuelve su primer elemento.
--Precondicion: -
headBis :: [a] -> a
headBis (x:xs) = x 

-- 4 C
--Proposito: Dada una lista devuelve esa lista menos el primer elemento.
--Precondicion: -
tailBis :: [a] -> [a]
tailBis (x:xs) = xs

-- Recursion:

-- 1
--Proposito: Dada una lista de enteros devuelve la suma de todos sus elementos.
--Precondicion:
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

sumatoria2 :: [Int] -> Int
sumatoria2 [] = 0
sumatoria2 (x:xs) = let ys = sumatoria xs
                    in x + ys

sumatoria3 :: [Int] -> Int
sumatoria3 xs = sum [x | x <- xs]

-- 2
--Proposito: Dada una lista de elementos de algun tipo devuelve el largo de esa lista, es decir, la cantidad 
--de elementos que posee.
--Precondicion: -
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

longitud2 :: [a] -> Int
longitud2 xs = sum [1 | _ <- xs]

-- 3
--Proposito: Dada una lista de enteros, devuelve un numero que es el promedio entre todos los elementos de la lista.
--Precondicion: -
promedio :: [Int] -> Int
promedio [] = 0
promedio xs = div (sumatoria xs) (longitud xs)

-- 4
--Proposito: Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
--Precondicion: -
mapSucesor :: [Int] -> [Int]
mapSucesor [] = []
mapSucesor (x:xs) = [x+1] ++ mapSucesor xs

mapSucesor2 :: [Int] -> [Int]
mapSucesor2 xs = [x+1 | x <- xs]

-- 5
--Proposito: Dada una lista de pares de enteros, devuelve una nueva lista en la que cada elemento es la suma de los elementos de cada par.
--Precondicion: -
mapSumaPar :: [(Int,Int)] -> [Int]
mapSumaPar [] = []
mapSumaPar (x:xs) = [sumaPar x] ++ mapSumaPar xs

-- 6
--Proposito: Dada una lista de pares, devuelve una nueva lista en la que cada elemento es el mayor de las componentes de cada par.
--Precondicion: -
mapMaxDelPar :: [(Int,Int)] -> [Int]
mapMaxDelPar [] = []
mapMaxDelPar (x:xs) = [maxDelPar x] ++ mapMaxDelPar xs

mapMaxDelPar2 :: [(Int,Int)] -> [Int]
mapMaxDelPar2 xs = [maxDelPar x | x <- xs]

-- 7
--Proposito: Dada una lista de booleanos devuelve True si todos sus elementos son True.
--Precondicion: -
todoVerdad :: [Bool] -> Bool
todoVerdad [] = True
todoVerdad (x:xs) = x && todoVerdad xs

todoVerdad2 :: [Bool] -> Bool
todoVerdad2 xs = longitud [x | x <- xs,x] == longitud xs

-- 8
--Proposito: Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
--Precondicion: -
algunaVerdad :: [Bool] -> Bool
algunaVerdad [] = False
algunaVerdad (x:xs) = x || algunaVerdad xs

-- 9 
--Proposito: Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
--Precondicion: -
pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs

-- 10
--Proposito: Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
--Precondicion: -
apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0 
apariciones e (x:xs) = (if (e == x) then 1 else 0) + apariciones e xs

-- 11
--Proposito: Dados un numero n y una lista xs, devuelve todos los elementos de xs que son menores a n.
--Precondicion: -
filtrarMenoresA :: Int -> [Int] -> [Int]
filtrarMenoresA n [] = []
filtrarMenoresA n (x:xs) = (if x<n then [x] else []) ++ filtrarMenoresA n xs

-- 12
--Proposito: Dados un elemento y una lista filtra (elimina) todas las ocurrencias de ese elemento en la lista.
--Precondicion: -
filtrarElemento :: Eq a => a -> [a] -> [a]
filtrarElemento e [] = []
filtrarElemento e (x:xs) = (if e==x then [] else [x]) ++ filtrarElemento e xs

-- 13
--Proposito: Dada una lista de listas, devuelve la lista de sus longitudes. Aplique esta funcion a la lista de strings 
--["Estructuras", "de", "datos"] y observe el resultado.
--Precondicion: 
mapLongitudes :: [[a]] -> [Int]
mapLongitudes [] = []
mapLongitudes (xs:xss) = (longitud xs):mapLongitudes xss

-- 14
--Proposito: Dados un numero n y una lista de listas, devuelve la lista de aquellas listas que tienen mas de n elementos.
--Precondicion: -
longitudMayorA :: Int -> [[a]] -> [[a]]
longitudMayorA n [] = []
longitudMayorA n (xs:xss) = (if longitud xs > n then [xs] else []) ++ longitudMayorA n xss 

-- 15 
--Proposito: Dado un elemento e y una lista xs, ubica a e entre medio de todos los elementos de xs.
--Ejemplo: intercalar ',' "abcde" == "a,b,c,d,e"
--Precondicion: -
intercalar :: a -> [a] -> [a]
intercalar e [] = []
intercalar e [x] = [x]
intercalar e (x:xs) = [x,e] ++ intercalar e xs 

-- 16 
--Proposito: Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la lista.
--Precondicion:
snoc :: [a] -> a -> [a]
snoc [] e = [e]
snoc (x:xs) e =  x:snoc xs e
--snoc xs e = xs ++ [e]

-- 17
--Proposito: Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
--elementos de la segunda a continuacion. Definida en Haskell como ++. 
--Precondicion:
append :: [a] -> [a] -> [a]
append [] ys = ys
append xs [] = xs 
append (x:xs) ys = x : append xs ys

-- 18
--Proposito: Dada una lista de listas, devuelve una unica lista con todos sus elementos.
--Precondicion:
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs:xss) = xs ++ aplanar xss

-- 19 
--Proposito: Dada una lista devuelve la lista con los mismos elementos de atras para adelante.
--Definida en Haskell como reverse.
--Precondicion:
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

-- 20
--Proposito: Dadas dos listas de enteros, devuelve una lista donde el elemento en la posicion n es el
--maximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
--las listas no necesariamente tienen la misma longitud.
--Precondicion:
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos xs [] = xs
zipMaximos [] ys = ys
zipMaximos (x:xs) (y:ys) = max x y : zipMaximos xs ys

-- 21
--Proposito: Dadas dos listas de enteros de igual longitud, devuelve una lista de pares (min, max), donde
--min y max son el minimo y el maximo entre los elementos de ambas listas en la misma posicion.
--Precondicion: Ambas listas deben tener la misma longitud.
zipSort :: [Int] -> [Int] -> [(Int, Int)]
zipSort [] [] = []
zipSort (x:xs) (y:ys) = (min x y,max x y) :zipSort xs ys

-- Recursion sobre numeros:

-- 1
--Proposito: Dado un numero n se devuelve la multiplicacíon de este numero y todos sus anteriores hasta
--llegar a 0. Si n es 0 devuelve 1. La funcion es parcial si n es negativo.
--Precondicion: n no debe ser negativo. 
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 2
--Proposito: Dado un numero n devuelve una lista cuyos elementos sean los numeros comprendidos entre
--n y 1 (incluidos). Si el numero es inferior a 1, devuelve la lista vacia.
--Precondicion: n no debe ser negativo. 
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = [n] ++ cuentaRegresiva (n-1)

-- 3
--Proposito: Dado un numero n devuelve una lista cuyos elementos sean los numeros entre 1 y n (incluidos)
--Precondicion: n no debe ser negarivo
contarHasta :: Int -> [Int]
contarHasta 0 = []
contarHasta n = contarHasta (n-1) ++ [n]
--contarHasta n = snoc n (contarHasta (n-1))

-- 4
--Proposito: Dado un numero n y un elemento e devuelve una lista en la que el elemento e repite n veces.
--Precondicion: 
replicarN :: Int -> a -> [a]
replicarN 0 _ = []
replicarN n e =  e:replicarN (n-1) e

-- 5
--Proposito: Dados dos numeros n y m devuelve una lista cuyos elementos sean los numeros entre n y m (incluidos)
--Precondicion: 
desdeHasta :: Int -> Int -> [Int]
desdeHasta n m = if n > m then [] else n : desdeHasta (n+1) m

-- 6
--Proposito: Dados un numero n y una lista xs, devuelve una lista con los primeros n elementos de xs.
--Si xs posee menos de n elementos, se devuelve la lista completa.
--Precondicion: n no debe ser negativo
takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

-- 7
--Proposito: Dados un numero n y una lista xs, devuelve una lista sin los primeros n elementos de lista
--recibida. Si la lista posee menos de n elementos, se devuelve una lista vaćıa.
--Precondicion: 
dropN :: Int -> [a] -> [a]
dropN n [] = []
dropN 0 xs = xs
dropN n (x:xs) = dropN (n-1) xs

-- 8
--Proposito: Dados un numero n y una lista xs, devuelve un par donde la primera componente es la lista
--que resulta de aplicar takeN a xs, y la segunda componente el resultado de aplicar dropN a xs. 
-- ¿Conviene utilizar recursion?
--Precondicion:
splitN :: Int -> [a] -> ([a], [a])
splitN n xs = (takeN n xs,dropN n xs)

-- Anexo con ejercicios adicionales:

-- 1
--Proposito: Dada una lista xs de enteros devuelva una tupla de listas, donde la primera componente 
--contiene todos aquellos numeros positivos de xs y la segunda todos aquellos numeros negativos de xs.
-- ¿Conviene utilizar recursion? Considere utilizar funciones auxiliares.
--Precondicion:
particionPorSigno :: [Int] -> ([Int], [Int])
particionPorSigno xs = (positivos xs,negativos xs)

-- Se queda con los positivos
positivos :: [Int] -> [Int]
positivos [] = []
positivos (x:xs) = (if x>=0 then [x] else []) ++ positivos xs

-- Se queda con los negativos
negativos :: [Int] -> [Int]
negativos [] = []
negativos (x:xs) = (if x<0 then [x] else []) ++ negativos xs

-- 2
--Proposito: Dada una lista xs de enteros devuelva una tupla de listas, donde la primera componente
--contiene todos aquellos numeros pares de xs y la segunda todos aquellos numeros impares de xs. 
-- ¿Conviene utilizar recursion? Considere utilizar funciones auxiliares.
--Precondicion:
particionPorParidad :: [Int] -> ([Int], [Int])
particionPorParidad xs = (pares xs,impares xs)

-- Se queda con los pares
pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) = (if (mod x 2)==1 then [] else [x]) ++ pares xs

-- Se queda con los impares
impares :: [Int] -> [Int]
impares [] = []
impares (x:xs) = (if (mod x 2)==1 then [x] else []) ++ impares xs

-- 3
--Proposito: Dada una lista devuelve cada sublista resultante de aplicar tail en cada paso. Ejemplo:
--subtails "abc" == ["abc", "bc", "c",""]
--Precondicion:
--subtails :: [a] -> [[a]]
--subtails [] = [[]]
--subtails xs = xs:subtails (tail xs)
subtails :: [a] -> [[a]]
subtails [] = [[]]
subtails (x:xs) = (x:xs):subtails xs

-- 4
--Proposito: Dada una lista xs devuelve una lista de listas donde cada sublista contiene elementos 
--contiguos iguales de xs. Ejemplo: agrupar "AABCCC" = ["AA","B","CC"]
agrupar2 :: Eq a => [a] -> [[a]]
agrupar2 [] = []
agrupar2 [x] = [[x]]
agrupar2 (x:y:xs) = if x==y then (x:head (agrupar2 (y:xs))):tail (agrupar2 (y:xs))
                            else [x]:agrupar2 (y:xs)

agrupar3 :: Eq a => [a] -> [[a]]
agrupar3 []     = []
agrupar3 (x:xs) = agrega3 x (agrupar3 xs)

agrega3 :: Eq a => a -> [[a]] -> [[a]]
agrega3 e []       = [[e]]
agrega3 e (xs:xss) = if e==(head xs)
                     	then (e:xs):xss
                     	else [e]:(xs:xss)

-- 5
--Proposito: Devuelve True si la primera lista es prefijo de la segunda.
--Precondicion: 
esPrefijo :: Eq a => [a] -> [a] -> Bool
esPrefijo [] _ =False
esPrefijo _ [] = False
esPrefijo (x:xs) (y:ys) = if x==y then True else False

-- 6
--Proposito: Devuelve True si la primera lista es sufijo de la segunda.
--Precondicion: 
esSufijo :: Eq a => [a] -> [a] -> Bool
esSufijo _ [] = False
esSufijo [] _ = False
esSufijo xs ys = if (last xs)==(last ys) then True else False

-- Ejercicio 6, Definir la función  agrupaN :: Int -> [a] -> [[a]]
-- tal que (agrupa n xs) es la lista de las sublistas de longitud n de la lista xs. Por ejemplo, 
-- agrupaN 2 [3,1,5,8,2,7] = [[3,1],[5,8],[2,7]]
-- agrupaN 2 [3,1,5,8,2,7,9] = [[3,1],[5,8],[2,7],[9]]
-- agrupaN 5 "todo necio confunde valor y precio" = ["todo ","necio"," conf","unde ","valor"," y pr","ecio"]
agrupaN :: Int -> [a] -> [[a]]
agrupaN n [] = []
agrupaN n xs = takeN n xs:agrupaN n (drop n xs)

-- Extras let
concatenarPares :: [(a,a)] -> ([a],[a])
concatenarPares []     = ([],[])
concatenarPares (x:xs) = let (ys,ps)=concatenarPares xs 
                         in (fst x:ys,snd x:ps)

--Proposito: Dada una lista xs devuelve una lista de listas donde cada sublista contiene elementos 
--contiguos iguales de xs. Ejemplo: agrupar "AABCCC" = ["AA","B","CC"]
agrupar :: Eq a => [a] -> [[a]]
agrupar []     = []
agrupar [x]    = [[x]]
agrupar (x:xs) = let l@(ys:yss) = agrupar xs
                 in if x==head ys 
                 	then (x:ys):yss
                 	else [x]:l 

-- ej [3,1,2,5,1] -> (1,[3,2,5,1]) Devuelve el minimo y despues se lo quita
-- la lista no es vacia
splitMin :: Ord a => [a] -> (a,[a])
splitMin [x]    = (x,[])
splitMin (x:xs) = let (y,ys)=splitMin xs
                  in if x<y then (x,y:ys) else (y,x:ys) 

-- Devuelve una lista con el minimo de cada lista. Prec: Ninguna sublista es vacia
minimo :: Ord a => [[a]] -> [a] 
minimo []       = []
minimo (xs:xss) = let ys=minimo xss
                  in minimum xs:ys

minimo2 :: Ord a => [[a]] -> [a] 
minimo2 []       = []
minimo2 (xs:xss) = let ys=minimo2 xss
                  in fst (splitMin xs):ys

ls :: [[Int]]
ls = [[1,2,3,4],[5,4,7,8],[1,45,7,1]]

p :: [(Int,Int)]
p = [(1,1),(2,4),(3,3)] 

