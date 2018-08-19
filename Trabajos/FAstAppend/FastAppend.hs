module FastAppend(FAList,TipTree,emptyF,isEmptyF,appendF,fl,t1,fxs,fll,tt,ttt,consF,headF,tailF) where
--module FastAppend where

data TipTree a = Join (TipTree a) (TipTree a) | Tip a 

data FAList a = Mkf (Maybe (TipTree a))

emptyF :: FAList a 
emptyF = Mkf Nothing

isEmptyF :: FAList a -> Bool
isEmptyF (Mkf Nothing) = True
isEmptyF    (Mkf _ )   = False

appendF :: FAList a -> FAList a -> FAList a 
appendF (Mkf x) (Mkf y) = Mkf (juntarF x y)

juntarF :: Maybe (TipTree a) -> Maybe (TipTree a) -> Maybe (TipTree a)
juntarF Nothing y = y
juntarF x Nothing = x
juntarF (Just x) (Just y) = Just (juntarTT x y)

juntarTT :: TipTree a -> TipTree a -> TipTree a
juntarTT x y = Join x y 

tt :: TipTree Int
tt = Join (Join (Join (Tip 1) (Tip 2)) (Join (Tip 3) (Tip 4))) (Join (Join (Tip 5) (Tip 6)) (Join (Tip 7) (Tip 8)))

ttt :: TipTree Int
ttt = Join (Join (Join (Tip 8) (Tip 3)) (Join (Tip 5) (Tip 4))) (Join (Join (Tip 2) (Tip 11)) (Join (Tip 7) (Tip 6)))

t1 :: TipTree [Int]
t1 = Join (Join (Join (Tip [1,2]) (Tip [3])) (Join (Tip [5]) (Tip [4]))) (Join (Join (Tip [2,10]) (Tip [11,12])) 
	(Join (Tip [7]) (Tip [6])))

fl :: FAList Int
fl = Mkf (Just tt)

fll :: FAList Int
fll = Mkf (Just ttt)

fxs :: FAList [Int]
fxs = Mkf (Just t1)

fe :: FAList Int
fe = Mkf Nothing

consF :: a -> FAList a -> FAList a
consF x (Mkf m) = Mkf (agregarF x m)

agregarF :: a -> Maybe (TipTree a) -> Maybe (TipTree a)
agregarF e Nothing  = Just (Tip e)
agregarF e (Just t) = Just (agregarTT e t)

agregarTT :: a -> TipTree a -> TipTree a
agregarTT e t = Join (Tip e) t 

headF :: FAList a -> a
headF (Mkf m) = cabezaF m 

cabezaF :: Maybe (TipTree a) -> a 
cabezaF Nothing  = error "boom"
cabezaF (Just e) = cabezaTT e 

cabezaTT :: TipTree a -> a 
cabezaTT   (Tip e)  = e
cabezaTT (Join x y) = cabezaTT x

tailF :: FAList a -> FAList a 
tailF (Mkf m) = Mkf (colaF m) 

colaF :: Maybe (TipTree a) -> Maybe (TipTree a) 
colaF Nothing        = error "boom"
colaF (Just (Tip x)) = Nothing
colaF (Just t)       = Just (colaTT t) 

colaTT :: TipTree a -> TipTree a
colaTT (Join (Tip x) y) = y 
colaTT (Join   x   y )  = Join (colaTT x) y



