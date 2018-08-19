module Persona where
-- 1
data Dir = Norte | Sur | Este | Oeste
--           deriving (Show,Eq,Ord)

-- Dada una direccion devuelve su opuesta
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur   = Norte
opuesto Oeste = Este
opuesto Este  = Oeste

-- Dada una direccion devuelve su siguiente, en sentido horario.
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Sur = Oeste
siguiente Oeste = Norte
siguiente Este = Sur

-- 2
-- data Persona = MkPersona String Int
data Persona = MkPersona Nombre Edad

type Nombre = String
type Edad = Int

-- Funciones constructores
nacer :: Nombre -> Persona 
nacer n = MkPersona n 0

-- A y B
-- Funciones de acceso:
edad :: Persona -> Edad
edad (MkPersona _ e) = e

nombre :: Persona -> Nombre
nombre (MkPersona n _) = n

-- c
crecer :: Persona -> Persona
crecer (MkPersona n e) = MkPersona n (e+1) 

--H Precondicion: La lista no puede ser vacia
elMasViejo :: [Persona] -> Persona
elMasViejo [p] = p
elMasViejo (p:ps) = elMayor p (elMasViejo ps)

elMayor :: Persona -> Persona -> Persona
elMayor p1 p2 = if p1>p2 then p1
                         else p2
--elMayor p1 p2 = if (Edad p1) > (Edad p2) then p1
--                                         else p2 
--elMayor (MkPersona n1 e1) (MkPersona n2 e2) = if e1>e2 then MkPersona n1 e1
--                                                       else MkPersona n2 e2

instance Ord Persona where
    p1 > p2 = (edad p1) > (edad p2)
    p1 < p2 = (edad p1) < (edad p2)
    p1 >= p2 = (edad p1) >= (edad p2)
    p1 <= p2 = (edad p1) <= (edad p2) 

instance Eq Persona where
    p1 == p2 = (edad p1)==(edad p2) && (nombre p1)==(nombre p2)

-- D
-- Dados un nombre y una persona, reemplaza el nombre de la persona por este otro.
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre s (MkPersona p e) = MkPersona s e

-- E
-- Dadas dos personas indica si la primera es m ́as joven que la segunda.
esMenorQueLaOtra :: Persona -> Persona -> Bool
esMenorQueLaOtra p1 p2 = if p1<p2 then True
                                  else False

-- F
-- Dados una edad y una lista de personas devuelve todas las personas que son mayores a esa edad.
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA n (p:ps) = if n<(edad p) then p:mayoresA n ps else mayoresA n ps

-- G
--Dada una lista de personas devuelve el promedio de edad entre esas personas. 
--La lista al menos posee una persona.
promedioEdad :: [Persona] -> Int
promedioEdad ps = div (sumarEdades ps) (length ps)

sumarEdades :: [Persona] -> Int
sumarEdades [] = 0
sumarEdades (p:ps) = (edad p) + sumarEdades ps

--Definir los tipos de datos Pokemon, como un TipoPokemon (agua, fuego o planta) y
--un porcentaje de energia; y Entrenador, como un nombre y una lista de Pokemon.
data Pokemon = MkPokemon TipoPokemon Energia 

data TipoPokemon = Fuego | Agua | Planta

data Entrenador = MkEntrenador String [Pokemon]

type Energia = Int

energia :: Pokemon -> Energia
energia (MkPokemon _ e) = e 

tipo :: Pokemon -> TipoPokemon
tipo (MkPokemon t _) = t 

instance Eq Pokemon where 
    p1 == p2 = (tipo p1) == (tipo p2) && (energia p1) == (energia p2)

instance Eq TipoPokemon where
    Agua == Agua = True
    Fuego == Fuego = True
    Planta == Planta = True
    _ == _ = False

-- A
-- Dado un TipoDePokemon devuelve el elemento que le gana a ese. 
-- Agua le gana a fuego, fuego a planta y planta a agua.
elementoGanador :: TipoPokemon -> TipoPokemon
elementoGanador Fuego = Agua
elementoGanador Planta = Fuego
elementoGanador Agua = Planta

-- B
-- Dados dos pokemon indica si el primero le puede ganar al segundo. Se considera que gana si
-- su elemento es opuesto al del otro pokemon. Si poseen el mismo elemento se considera que no gana.
leGanaA :: Pokemon -> Pokemon -> Bool
leGanaA p1 p2 = (tipo p1) == (elementoGanador (tipo p2))

--Proposito: Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
--Precondicion: -
apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0 
apariciones e (x:xs) = (if (e == x) then 1 else 0) + apariciones e xs

-- Dado un Entrenador y un Pokemon, retornar la cantidad de Pokemons iguales al Pokemon dado que tenga el entrenador
cantidadDePokemonIgualesA :: Entrenador -> Pokemon -> Int
cantidadDePokemonIgualesA e p = apariciones p (pokemons e)

pokemons :: Entrenador -> [Pokemon]
pokemons (MkEntrenador _ ps) = ps

-- C
--Agrega un pokemon a la lista de pokemon del entrenador.
capturarPokemon :: Pokemon -> Entrenador -> Entrenador
capturarPokemon p (MkEntrenador n ps) = MkEntrenador n (p:ps)

-- D
--Devuelve la cantidad de pokemons que posee el entrenador.
cantidadDePokemons :: Entrenador -> Int
cantidadDePokemons e = length (pokemons e) 

-- E
--Devuelve la cantidad de pokemons de determinado tipo que posee el entrenador.
cantidadDePokemonsDeTipo :: TipoPokemon -> Entrenador -> Int
cantidadDePokemonsDeTipo _ (MkEntrenador _ []) = 0
cantidadDePokemonsDeTipo t (MkEntrenador n (p:ps)) = (if t==(tipo p) then 1
                                                                     else 0) 
                                                   + cantidadDePokemonsDeTipo t (MkEntrenador n ps)

-- F
--Dados un entrenador y un pokemon devuelve True si el entrenador posee un pokemon que le gane ese pokemon.
lePuedeGanar :: Entrenador -> Pokemon -> Bool
lePuedeGanar e p = (cantidadDePokemonsDeTipo (elementoGanador (tipo p)) e)>0
-- lePuedeGanar (MkEntrenador "aa" [MkPokemon Agua 1,MkPokemon Agua 0,MkPokemon Fuego 1]) (MkPokemon Fuego 1)

-- G
--Dados un tipo de pokemon y dos entrenadores, devuelve True si ambos entrenadores
--tiene al menos un pokemon de ese tipo y que tenga energia para pelear.
puedenPelear :: TipoPokemon -> Entrenador -> Entrenador -> Bool
puedenPelear t e1 e2 = (tipoConEnergia t e1) && (tipoConEnergia t e2)
-- puedenPelear Agua (MkEntrenador "aaa" [MkPokemon Agua 1,MkPokemon Agua 0,MkPokemon Fuego 1])
-- (MkEntrenador "bbb" [MkPokemon Fuego 1,MkPokemon Agua 0,MkPokemon Planta 1])

--Dados un tipo de Pokemon y un entrenador, debuelve True si tiene al menos un pokemon de ese tipo 
-- y que tenga energia para pelear.
tipoConEnergia :: TipoPokemon -> Entrenador -> Bool
tipoConEnergia t (MkEntrenador _ []) = False 
tipoConEnergia t (MkEntrenador n (p:ps)) = ((tipo p)==t && (energia p)>0) || tipoConEnergia t (MkEntrenador n ps)
                                            
-- H 
--Dado un entrenador devuelve True si ese entrenador posee al menos un pokemon de cada tipo posible.
esExperto :: Entrenador -> Bool
esExperto e = (cantidadDePokemonsDeTipo Agua e)>0 &&
              (cantidadDePokemonsDeTipo Fuego e)>0 &&
              (cantidadDePokemonsDeTipo Planta e)>0

esExpertoBis :: Entrenador -> Bool
esExpertoBis e = esExpertoLista e ([Agua,Fuego,Planta])

esExpertoLista :: Entrenador -> [TipoPokemon] -> Bool
esExpertoLista _ [] = False
esExpertoLista e (t:ts) = not ((not (cantidadDePokemonsDeTipo t e >0)) || esExpertoLista e ts)  
--esExperto (MkEntrenador "aa" [MkPokemon Fuego 1,MkPokemon Agua 0,MkPokemon Planta 1])

-- I
--Dada una lista de entrenadores devuelve una lista con todos los pokemon de cada entrenador.
fiestaPokemon :: [Entrenador] -> [Pokemon]
fiestaPokemon [] = []
fiestaPokemon (e:es) = pokemons e ++ fiestaPokemon es 
--fiestaPokemon [(MkEntrenador "aaa" [MkPokemon Agua 1,MkPokemon Agua 0,MkPokemon Fuego 1]),
-- (MkEntrenador "bbb" [MkPokemon Agua 10,MkPokemon Agua 10,MkPokemon Fuego 10])]









