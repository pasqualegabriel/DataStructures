
import Map
--Map, emptyM, assocM, lookupM, removeM, domM

data Color = Azul | Negro | Rojo | Verde

data Celda = MkCelda (Map Color Int)

instance Eq Color where
	Azul == Azul = True
	Negro == Negro = True
	Rojo == Rojo = True
	Verde == Verde = True
	x == y = False

celdaVacia :: Celda
celdaVacia = MkCelda emptyM

poner :: Color -> Celda -> Celda
poner c (MkCelda map) = MkCelda (assocM map c (sumarUno (lookupM map c)))

sumarUno :: Maybe Int -> Int
sumarUno Nothing  = 1
sumarUno (Just x) = x+1

sacar :: Color -> Celda -> Celda
sacar c (MkCelda map) = MkCelda (actualizarSacarUno map c (lookupM map c))

actualizarSacarUno :: Map Color Int -> Color -> Maybe Int -> Map Color Int
actualizarSacarUno map c Nothing  = map   
actualizarSacarUno map c (Just n) = if n==0 || n==1
	                                then removeM map c 
	                                else assocM map c (n-1)

nroBolitas :: Color -> Celda -> Int 
nroBolitas c (MkCelda map) = cantBolitas (lookupM map c)

cantBolitas :: Maybe Int -> Int 
cantBolitas Nothing  = 0
cantBolitas (Just n) = n 

hayBolitas :: Color -> Celda -> Bool
hayBolitas c m = (nroBolitas c m)>0  






















