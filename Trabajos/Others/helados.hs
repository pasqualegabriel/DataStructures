
data Gusto = Chocolate | Sambayon | Dulce | Frutilla | Menta
             

data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote [Gusto]
             

precioG :: Gusto -> Int
precioG Frutilla = 2
precioG Menta = 2
precioG _ = 1

precioH :: Helado -> Int
precioH (Vasito g) = 10 + precioG g
precioH (Cucurucho g1 g2) = 20 + precioG g1 + precioG g2
precioH (Pote gs) = 30 + precioListaH gs

precioListaH :: [Gusto] -> Int
precioListaH [] = 0
precioListaH (g:gs) = precioG g + precioListaH gs

-- Transforma un helado en un helado en serio cambiando Frutilla por Chocolate y Menta por Sambayon
enSerio :: Helado -> Helado
enSerio (Vasito g) = Vasito (gustoEnSerio g)
enSerio (Cucurucho g1 g2) = Cucurucho (gustoEnSerio g1) (gustoEnSerio g2)
enSerio (Pote gs) = Pote (gustoEnSerioLista gs)

gustoEnSerio :: Gusto -> Gusto
gustoEnSerio Frutilla = Chocolate
gustoEnSerio Menta = Sambayon
gustoEnSerio g = g

gustoEnSerioLista :: [Gusto] -> [Gusto]
gustoEnSerioLista [] = []
gustoEnSerioLista (g:gs) = gustoEnSerio g : gustoEnSerioLista gs 










