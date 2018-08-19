
data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a)

data Dir = Izq | Der

data Celda = ConsCelda Int Int

instance Eq Dir where
    Izq == Izq = True
    Der == Der = True
    _  ==  _   = False

-- Nota: El orden de las bolitas en el constructor ConsCelda es primero verdes y luego rojas.

-- 1
--Dado un camino y un arbol, indica si ese camino existe en el arbol. Un camino existe si puedo completarlo sin
--que se termine el arbol.
existeCamino :: [Dir] -> Arbol Celda -> Bool
existeCamino [] _ = True
existeCamino _ Vacio = False
existeCamino (Der:xs) (Nodo a t1 t2) = existeCamino xs t2
existeCamino (Izq:xs) (Nodo a t1 t2) = existeCamino xs t1
--existeCamino (x:xs) (Nodo a t1 t2) = if (x==Der) then existeCamino xs t2
--	                                             else existeCamino xs t1
--existeCamino [Izq,Der,Der] (Nodo (ConsCelda 1 1) (Nodo (ConsCelda 2 2) Vacio (Nodo (ConsCelda 3 3) Vacio Vacio)) Vacio)

-- 2
--Dado un camino y un arbol, retorna la cantidad de bolitas rojas que posee la celda al final del camino.
rojasDeCelda :: [Dir] -> Arbol Celda -> Int
rojasDeCelda [] Vacio              = 0
rojasDeCelda [] (Nodo a t1 t2)     = cantBolitasRojas a
rojasDeCelda (x:xs) (Nodo a t1 t2) = if x==Der then rojasDeCelda xs t2 
                                               else rojasDeCelda xs t1

-- Dada una Celda retorna la cantidad de Bolitas Rojas.
cantBolitasRojas :: Celda -> Int
cantBolitasRojas (ConsCelda _ n2) = n2   
--rojasDeCelda [Izq,Der] (Nodo (ConsCelda 1 1) (Nodo (ConsCelda 2 2) Vacio (Nodo (ConsCelda 3 3) Vacio Vacio)) Vacio)

-- 3
-- Dado un arbol de celdas retorna la celda que tenga mas bolitas rojas.
celdaConMasRojas :: Arbol Celda -> Celda
celdaConMasRojas Vacio          = ConsCelda 0 0
celdaConMasRojas (Nodo a t1 t2) = rojasMayorCelda a (rojasMayorCelda (celdaConMasRojas t1) (celdaConMasRojas t2)) 

celdaConMasRojas2 :: Arbol Celda -> Celda
celdaConMasRojas2 Vacio          = ConsCelda 0 0
celdaConMasRojas2 (Nodo a t1 t2) = rojasMayorCelda (rojasMayorCelda a (celdaConMasRojas2 t1))
                                                   (rojasMayorCelda a (celdaConMasRojas2 t1))

-- Dadas dos Celdas devuelve la Celda con mas cantidad de Bolitas Rojas.
rojasMayorCelda :: Celda -> Celda -> Celda
rojasMayorCelda (ConsCelda n1 n2) (ConsCelda b1 b2) = if n2>b2 then ConsCelda n1 n2 else ConsCelda b1 b2

celdaConMasRojas3 :: Arbol Celda -> Celda
celdaConMasRojas3 Vacio = ConsCelda 0 0
celdaConMasRojas3 t     = celdaRojas (listaArbolCelda t)

-- Dado un arbol de Celda retorna una lista con todas sus raizes
listaArbolCelda :: Arbol Celda -> [Celda]
listaArbolCelda Vacio          = []
listaArbolCelda (Nodo a t1 t2) = (a:listaArbolCelda t1) ++ listaArbolCelda t2 

-- Dada una lista de Celda, devuelve la Celda con mas cantidad de Bolitas Rojas
-- Precdondicion: La lista de Celda no debe ser vacio (not(null [Celda]))
-- La precondicion me la asegura el caso vacio de celdaConMasRojas.
celdaRojas :: [Celda] -> Celda
celdaRojas [x]    = x
celdaRojas (x:xs) = rojasMayorCelda x (celdaRojas xs)
-- celdaConMasRojas (Nodo (ConsCelda 1 1) (Nodo (ConsCelda 2 2) Vacio (Nodo (ConsCelda 3 3) Vacio Vacio)) Vacio) 

-- 4
-- Dada una lista de caminos, vacía las celdas que se encuentren al final de dichos caminos.
vaciarCeldas :: [[Dir]] -> Arbol Celda -> Arbol Celda
vaciarCeldas [] t       = t
vaciarCeldas xss Vacio  = Vacio
vaciarCeldas (xs:xss) t = recorrerYVaciar xs (vaciarCeldas xss t) 

-- Dada una lista de caminos y un arbol de Celda, vacía la Celda que se encuentra al final del camino.
recorrerYVaciar :: [Dir] -> Arbol Celda -> Arbol Celda
recorrerYVaciar _ Vacio               = Vacio
recorrerYVaciar [] (Nodo a t1 t2)     = Nodo (emptyCelda a) t1 t2
recorrerYVaciar (x:xs) (Nodo a t1 t2) = if x==Der then Nodo a t1 (recorrerYVaciar xs t2)
                                                  else Nodo a (recorrerYVaciar xs t1) t2

recorrerYVaciar2 :: [Dir] -> Arbol Celda -> Arbol Celda
recorrerYVaciar2 (Izq:xs) (Nodo a t1 t2) = Nodo a (recorrerYVaciar2 xs t1) t2
recorrerYVaciar2 (Der:xs) (Nodo a t1 t2) = Nodo a t1 (recorrerYVaciar2 xs t2)
recorrerYVaciar2 [] (Nodo a t1 t2)       = Nodo (emptyCelda a) t1 t2 

-- Dada una Celda, la vacía
emptyCelda :: Celda -> Celda
emptyCelda (ConsCelda _ _) = ConsCelda 0 0 
--vaciarCeldas [[Izq,Der],[Izq,Izq]] (Nodo (ConsCelda 1 1) (Nodo (ConsCelda 2 2) (Nodo (ConsCelda 3 3) 
--Vacio Vacio) (Nodo (ConsCelda 4 4) Vacio Vacio)) Vacio) 
--recorrerYVaciar [Izq,Der] (Nodo (ConsCelda 1 1) (Nodo (ConsCelda 2 2) (Nodo (ConsCelda 3 3) 
--Vacio Vacio) (Nodo (ConsCelda 4 4) Vacio Vacio)) Vacio) 

-- 5
-- Dado un arbol retorna el camino mas largo desde la raiz hasta alguna de sus hojas, o, en otras palabras,
-- el camino desde la raiz hasta la hoja mas lejana.
caminoMasLargo :: Arbol Celda -> [Dir]
caminoMasLargo Vacio = []
caminoMasLargo t     = mayorLongitud (todosLosCaminos t)

mayorLongitud :: [[Dir]] -> [Dir]
mayorLongitud []       = []
mayorLongitud (xs:xss) = longitudMayorLss xs (mayorLongitud xss)

longitudMayorLss :: [a] -> [a] -> [a]
longitudMayorLss xs ys = if (length xs) > (length ys) then xs else ys

todosLosCaminos :: Arbol Celda -> [[Dir]]
todosLosCaminos Vacio                = []
todosLosCaminos (Nodo a Vacio Vacio) = [[]]
todosLosCaminos (Nodo a t1 t2)       = 
	(agregarCadaSublista Izq (todosLosCaminos t1)) ++ (agregarCadaSublista Der (todosLosCaminos t2))

agregarCadaSublista :: Dir -> [[Dir]] -> [[Dir]]
agregarCadaSublista d []       = [] 
agregarCadaSublista d (xs:xss) = (d:xs):agregarCadaSublista d xss
--todosLosCaminos (Nodo (ConsCelda 1 1) (Nodo (ConsCelda 2 2) Vacio (Nodo (ConsCelda 3 3) Vacio Vacio)) 
--(Nodo (ConsCelda 2 2) (Nodo (ConsCelda 3 3) (Nodo (ConsCelda 4 4) Vacio Vacio) Vacio) Vacio)) 

-- agrupar
agrupar :: Eq a => [a] -> [[a]]
agrupar [] = []
agrupar (x:xs) = aregarE x (agrupar xs)

aregarE :: Eq a => a -> [[a]] -> [[a]]
aregarE e []             = [[e]]
aregarE e l@((x:xs):xss) = if e==x then (e:(x:xs)):xss
                                   else [e]:l

-- multiplicar por dos el ultimo elemento de la lista
mulDosLs :: [Int] -> [Int] 
mulDosLs [x]    = [x*2]
mulDosLs (x:xs) = x:(mulDosLs xs)
--mulDosLs xs = init xs ++ [(last xs)*2]
--mulDosLs [1,2,3,4]

-- Tema 2
-- 1 Dado un camino y un arbol, retorna la cantidad total de bollitas que posee la celda al final del camino
totalDeCelda :: [Dir] -> Arbol Celda -> Int
totalDeCelda [] Vacio                = 0
totalDeCelda [] (Nodo a t1 t2)       = cantBolitas a 
totalDeCelda (Izq:xs) (Nodo a t1 t2) = totalDeCelda xs t1
totalDeCelda (Der:xs) (Nodo a t1 t2) = totalDeCelda xs t2

cantBolitas :: Celda -> Int
cantBolitas (ConsCelda n1 n2) = n1 + n2
--totalDeCelda [Izq,Der] (Nodo (ConsCelda 1 1) (Nodo (ConsCelda 2 2) Vacio (Nodo (ConsCelda 3 3) Vacio Vacio)) Vacio) 

-- 2 Dado un arbol de celdas retorna la celda que tenga menos bolitas verdes.
-- Precondicion: El arbol no debe ser vacio
celdaConMenosVerdes :: Arbol Celda -> Celda
celdaConMenosVerdes t = menosVerdesLista (listaDeCeldas t)
--celdaConMenosVerdes (Nodo (ConsCelda 1 1) (Nodo (ConsCelda 2 2) Vacio (Nodo (ConsCelda 3 3) Vacio Vacio)) Vacio)

menosVerdesLista :: [Celda] -> Celda 
menosVerdesLista [x]    = x
menosVerdesLista (x:xs) = celdaMenorVerde x (menosVerdesLista xs)

celdaMenorVerde :: Celda -> Celda -> Celda
celdaMenorVerde c1 c2 = if cantVerdes c1 < cantVerdes c2 then c1 else c2

cantVerdes :: Celda -> Int
cantVerdes (ConsCelda n1 n2) = n1

listaDeCeldas :: Arbol Celda -> [Celda]
listaDeCeldas Vacio          = []
listaDeCeldas (Nodo a t1 t2) = a:(listaDeCeldas t1 ++ listaDeCeldas t2)
--listaDeCeldas (Nodo (ConsCelda 1 1) (Nodo (ConsCelda 2 2) Vacio (Nodo (ConsCelda 3 3) Vacio Vacio)) Vacio) 

-- 3 Dada una lista de caminos indica si las celdas de esos caminos estan vacias.
caminosVacios :: [[Dir]] -> Arbol Celda -> Bool 
caminosVacios [] t   = True
caminosVacios (x:xs) t = (recorrerYVacias x t)  && (caminosVacios xs t)

recorrerYVacias :: [Dir] -> Arbol Celda -> Bool 
recorrerYVacias [] Vacio                = True
recorrerYVacias [] (Nodo a t1 t2)       = (cantBolitas a == 0)
recorrerYVacias (Izq:xs) (Nodo a t1 t2) = recorrerYVacias xs t1
recorrerYVacias (Der:xs) (Nodo a t1 t2) = recorrerYVacias xs t2
--recorrerYVacias (Nodo (ConsCelda 1 1) (Nodo (ConsCelda 2 2) Vacio (Nodo (ConsCelda 3 3) Vacio Vacio)) Vacio) 

-- 4 Devuelve la cantidad de bolitas verdes de cada celda que recorre por el camino
cantVerdesPorCamino :: [Dir] -> Arbol Celda -> [Int]
cantVerdesPorCamino [] t                    = []
cantVerdesPorCamino (Izq:xs) (Nodo a t1 t2) = (cantBolitasVerdes a) : (cantVerdesPorCamino xs t1)
cantVerdesPorCamino (Der:xs) (Nodo a t1 t2) = (cantBolitasVerdes a) : (cantVerdesPorCamino xs t2)

cantBolitasVerdes :: Celda -> Int
cantBolitasVerdes (ConsCelda n1 n2) = n1
--cantVerdesPorCamino (Nodo (ConsCelda 1 1) (Nodo (ConsCelda 2 2) Vacio (Nodo (ConsCelda 3 3) Vacio Vacio)) Vacio) 

--todosLosCaminos :: Tree a -> [[a]]
--todosLosCaminos EmptyT = []
--todosLosCaminos (NodeT x EmptyT EmptyT) = [[x]]
--todosLosCaminos (NodeT x t1 t2) = agregarACadaSublista x (todosLosCaminos t1 ++ todosLosCaminos t2)

--agregarACadaSublista :: a -> [[a]] -> [[a]]
--agregarACadaSublista y [] = []
--agregarACadaSublista y (xs:xss) = (y:xs):agregarACadaSublista y xss
































