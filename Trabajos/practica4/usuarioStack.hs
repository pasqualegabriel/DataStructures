import Stack
{- 
Stack (pila)
Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, first
out). Esto significa que los últimos elementos agregados a la estructura son los
primeros en salir (como en una pila de platos). Su interfaz es la siguiente:

emptyS :: Stack a
Crea una pila vacía.

isEmptyS :: Stack a -> Bool
Dada una pila indica si está vacía.

push :: a -> Stack a -> Stack a
Dados un elemento y una pila, agrega el elemento a la pila.

top :: Stack a -> a
Dada un pila devuelve el elemento del tope de la pila.

pop :: Stack a -> Stack a
Dada una pila devuelve la pila sin el primer elemento.
-}
-- Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar :: [a] -> Stack a
apilar xs = apilando (reversa xs)

apilando :: [a] -> Stack a
apilando []     = emptyS
apilando (x:xs) = push x (apilando xs)

-- Toma un string que representa una expresión aritmética, por ejemplo "(2+3)*2", 
-- y verifica que la cantidad de paréntesis que abren se corresponda
-- con los que cierran. Para hacer esto utilice una stack. Cada
-- vez que encuentra un paréntesis que abre, lo apila. Si encuentra un
-- paréntesis que cierra desapila un elemento. Si al terminar de recorrer
-- el string se desapilaron tantos elementos como los que se apilaron, ni
-- más ni menos, entonces los paréntesis están balaceados.
balanceado :: String -> Bool
balanceado s = (siAbre (apilar s)) == (siCierra (apilar s)) 

siAbre :: Stack Char -> Int
siAbre s = if isEmptyS s then 0 else abre (top s) + siAbre (pop s)

siCierra :: Stack Char -> Int
siCierra s = if isEmptyS s then 0 else cierra (top s) + siCierra (pop s)

abre :: Char -> Int
abre c = if c=='(' then 1 else 0

cierra :: Char -> Int
cierra c = if c==')' then 1 else 0

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

l :: [Int]
l = [1,2,3,4]

x :: Stack Char
x = MkS "(gh)gh(hj)f"

a :: Stack Int
a = apilar l






















