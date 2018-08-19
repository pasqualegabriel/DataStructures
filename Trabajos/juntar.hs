-- Parcial 20%

-- Mundial de futbol
-- Un mundial es un arbol de partidos
-- La final es la hoja del arbol y el ganador de dicho partido es el campeon
-- Un partido esta representado con el constructor MkPartido, el primer Equipo
--  es el Equipo local y el segundo el visitante, el primer Int es la cantidad de
--  goles que marco el equipo local y el segundo el visitante
-- Este mundial es solo de eliminacion directa, avanza el equipo ganador y no 
--  pueden haber un empate

data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a)

data Equipo = Argentina | Brasil | Uruguay | Paraguay | Venezuela | Peru | Chile | Bolivia

data Partido = MkPartido Equipo Int Equipo Int

instance Eq Equipo where
    Argentina == Argentina = True
    Brasil    == Brasil    = True
    Uruguay   == Uruguay   = True
    Paraguay  == Paraguay  = True
    Venezuela == Venezuela = True
    Peru      == Peru      = True
    Chile     == Chile     = True
    Bolivia   == Bolivia   = True
    _         == _         = False

mundial :: Arbol Partido
mundial = Nodo p1 (Nodo p2 (Nodo p4 Vacio Vacio) (Nodo p6 Vacio Vacio)) 
                  (Nodo p3 (Nodo p5 Vacio Vacio) (Nodo p7 Vacio Vacio))

p1 :: Partido
p1 = MkPartido Argentina 2 Brasil 3

p2 :: Partido
p2 = MkPartido Argentina 2 Uruguay 1

p3 :: Partido
p3 = MkPartido Brasil 4 Chile 2

p4 :: Partido
p4 = MkPartido Argentina 2 Venezuela 1

p5 :: Partido
p5 = MkPartido Peru 2 Brasil 3

p6 :: Partido
p6 = MkPartido Uruguay 1 Bolivia 0

p7 :: Partido
p7 = MkPartido Paraguay 1 Chile 3

mismoEquipo :: Equipo -> Equipo -> Bool
mismoEquipo e1 e2 = e1==e2  

-- A
campeon :: Arbol Partido -> Equipo
campeon (Nodo a t1 t2) = ganador a

ganador :: Partido -> Equipo
ganador (MkPartido e1 n1 e2 n2) = if n1>n2 then e1 else e2

-- B 
totalDePartidos :: Arbol Partido -> Int
totalDePartidos Vacio          = 0 
totalDePartidos (Nodo a t1 t2) = 1 + totalDePartidos t1 + totalDePartidos t2

-- C
totalDePartidosDe :: Equipo -> Arbol Partido -> Int
totalDePartidosDe e Vacio          = 0
totalDePartidosDe e (Nodo a t1 t2) = jugoPartido e a + totalDePartidosDe e t1 + totalDePartidosDe e t2

jugoPartido :: Equipo -> Partido -> Int
jugoPartido e (MkPartido e1 n1 e2 n2) = 
	if mismoEquipo e e1 || mismoEquipo e e2 then 1 else 0

-- D 
golesDe :: Equipo -> Arbol Partido -> (Int,Int)
golesDe e Vacio          = (0,0)
golesDe e (Nodo a t1 t2) = sumarTuplas (golesAFyEC e a) (sumarTuplas (golesDe e t1) (golesDe e t2))

golesAFyEC :: Equipo -> Partido -> (Int,Int)
golesAFyEC e (MkPartido e1 n1 e2 n2) = if mismoEquipo e e1 then (n1,n2) else 
                                      (if mismoEquipo e e2 then (n2,n1) else (0,0)) 

sumarTuplas :: (Int,Int) -> (Int,Int) -> (Int,Int)
sumarTuplas (a,b) (c,d) = (a+c,b+d)

-- E 
partidoConMasGoles :: Arbol Partido -> Partido
partidoConMasGoles Vacio                = error "el arbol no puede ser vacio" 
partidoConMasGoles (Nodo a Vacio Vacio) = a
partidoConMasGoles (Nodo a a1 a2)       = masGoles (masGoles a (partidoConMasGoles a1)) (partidoConMasGoles a2)

partidoConMasGoles2 :: Arbol Partido -> Partido
partidoConMasGoles2 t = partidoMasGoles (listaDePartidos t)

listaDePartidos :: Arbol Partido -> [Partido]
listaDePartidos Vacio          = [] 
listaDePartidos (Nodo a a1 a2) = [a] ++ listaDePartidos a1 ++ listaDePartidos a2

partidoMasGoles :: [Partido] -> Partido
partidoMasGoles []     = error "la lista no puede ser vacia"
partidoMasGoles [x]    = x
partidoMasGoles (x:xs) = masGoles x (partidoMasGoles xs)

masGoles :: Partido -> Partido -> Partido
masGoles p1 p2 = if golesP p1 > golesP p2 then p1 else p2

golesP :: Partido -> Int
golesP (MkPartido e1 n1 e2 n2) = n1 + n2

-- F 
--Dado un equipo y un arbol de partidos, retorna el primer partido en que el equipo marco 1 solo gol
-- en caso de que no haya marcado un gol en algun partido retorna Nothing
-- Maybe ya viene definido en haskell
-- data Maybe a = Nothing | Just a
golUnicoDe :: Equipo -> Arbol Partido -> Maybe Partido
golUnicoDe e Vacio                = Nothing
golUnicoDe e (Nodo a Vacio Vacio) = golUnico e a 
golUnicoDe e (Nodo a t1 t2)       = elPrimer (golUnico e a) (elPrimer (golUnicoDe e t1) (golUnicoDe e t2))

golUnico :: Equipo -> Partido -> Maybe Partido
golUnico e p@(MkPartido e1 n1 e2 n2) = if (mismoEquipo e e1 &&(n1==1)) || (mismoEquipo e e2 &&(n2==1))
                                       then Just p else Nothing

elPrimer :: Maybe Partido -> Maybe Partido -> Maybe Partido
elPrimer Nothing Nothing = Nothing
elPrimer t1 Nothing      = t1 
elPrimer Nothing t2      = t2 
elPrimer t1 t2           = t1 

-- G
-- Dado un mundial, retorna la lista de todos los equipos que participaron en el mundial sin repetidos

juntarEquipos :: [Equipo] -> [Equipo] -> [Equipo] 
juntarEquipos [] ys     = ys 
juntarEquipos (x:xs) ys = agregarSiNoEsta x (juntarEquipos xs ys)

agregarSiNoEsta :: Equipo -> [Equipo] -> [Equipo]
agregarSiNoEsta e xs = if pertenece e xs then xs else e:xs 

pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs

-- Se puede utilizar la funcion ya creada juntarEquipos que dada dos lista de equipos, cada una SIN REPETIDOS
--  retorna la lista con todos los equipos de ambas listas sin repetidos  

listaDeEquipos :: Arbol Partido -> [Equipo] 
listaDeEquipos     Vacio      = []
listaDeEquipos (Nodo p t1 t2) = 
   juntarEquipos (equiposDelPartido p) (juntarEquipos (listaDeEquipos t1) (listaDeEquipos t2))

equiposDelPartido :: Partido -> [Equipo]
equiposDelPartido (MkPartido e1 n1 e2 n2)= [e1]++[e2]









