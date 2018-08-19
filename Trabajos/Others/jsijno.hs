data Animal = Jalder | Snarl | Huffy | Sloshy 

data Caracteristica = Feroz | ConCuernos | Sigloso

data ArbolDeAnimales = Preguntar Caracteristica ArbolDeAnimales ArbolDeAnimales
                     | Arriesgar Animal

-- juego1 = Arriesgar Snarl

-- juego2 = Preguntar Feroz (Arriesgar Jalder) (Arriesgar Sloshy)

-- juego3 = Preguntar Feroz (Preguntar Sigloso (Arriesgar Snarl)
-- 	                                           (Arriesgar Jalder))
--                       (Preguntar ConCuernos (Arriesgar Huffy)
--	                                           (Arriesgar Sloshy))
-- caracDeCadaUno juego1 = [(Snarl, [])]

-- caracDeCadaUno juego2 = [(Jalder, [Feroz]),(Sloshy,[])]

-- caracDeCadaUno juego3 = [(Snarl,  [Feroz,Sigloso]), 
--                          (Jalder, [Feroz]),
--                          (Huffy,  [ConCuernos]),
--                          (Sloshy, [])
--                         ]

caracDeCadaUno :: ArbolDeAnimales -> [(Animal, [Caracteristica])]
caracDeCadaUno (Arriesgar a)         = [(a, [])]
caracDeCadaUno (Preguntar c jsi jno) = agregarATodos c (caracDeCadaUno jsi) ++ caracDeCadaUno jno

agregarATodos :: Caracteristica -> [(Animal, [Caracteristica])] -> [(Animal, [Caracteristica])]
agregarATodos c []       = []
agregarATodos c (ac:acs) = (agregar c ac) : (agregarATodos c acs)

agregar :: Caracteristica -> (Animal, [Caracteristica]) -> (Animal, [Caracteristica])
agregar c (a,cs) = (a,c:cs)
-- caracDeCadaUno (Preguntar Feroz (Preguntar Sigloso (Arriesgar Snarl) (Arriesgar Jalder)) (Preguntar ConCuernos 
-- (Arriesgar Huffy) (Arriesgar Sloshy)))

cualesAnimalesHay :: ArbolDeAnimales -> [Animal]
cualesAnimalesHay (Arriesgar a)         = [a]
cualesAnimalesHay (Preguntar c jsi jno) = cualesAnimalesHay jsi ++ cualesAnimalesHay jno 


































