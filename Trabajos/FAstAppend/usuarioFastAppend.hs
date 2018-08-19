import FastAppend

concatList :: FAList [a] -> FAList a
concatList fs = if not (isEmptyF fs)
                then agregarAFalist (todosLosADe fs)
                else emptyF 

todosLosADe :: FAList [a] -> [a]
todosLosADe f = if not (isEmptyF f)
	            then headF f ++ todosLosADe (tailF f)
	            else []

agregarAFalist :: [a] -> FAList a 
agregarAFalist   []   = emptyF
agregarAFalist (x:xs) = consF x (agregarAFalist xs)

findMin :: Ord a => FAList a -> Maybe a 
findMin f = if not (isEmptyF f) 
	        then if not (isEmptyF (tailF f)) 
	        	 then Just (min (headF f) (fromJust (findMin (tailF f)))) 
	        	 else Just (headF f)
	        else Nothing

fromJust :: Maybe a -> a 
fromJust (Just x) = x 



