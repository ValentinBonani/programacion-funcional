-- aAaAaaaAh

data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa deriving Show

piza = Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza))

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen _ Prepizza = 0
cantidadCapasQueCumplen f (Capa i p) = fromEnum (f i) + cantidadCapasQueCumplen f p

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas _ Prepizza = Prepizza
conCapasTransformadas f (Capa i p) =  Capa (f i) (conCapasTransformadas f p)

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue _ Prepizza = Prepizza
soloLasCapasQue f (Capa i p) =  (if f i then Capa i else id) (soloLasCapasQue f p)

-- Ejercio 2

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _ = False
 
sinLactosa :: Pizza -> Pizza
sinLactosa = soloLasCapasQue (not . esQueso)

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa = (==0) . cantidadCapasQueCumplen esQueso

cantidadDeQueso :: Pizza -> Int
cantidadDeQueso = cantidadCapasQueCumplen esQueso

aceitunaDuplicator :: Ingrediente -> Ingrediente
aceitunaDuplicator (Aceitunas i) = Aceitunas (i*2)
aceitunaDuplicator i = i

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas = conCapasTransformadas aceitunaDuplicator

-- Ejercicio 3

pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada _ z Prepizza = z
pizzaProcesada f z (Capa i p) = f i (pizzaProcesada f z p)

-- Ejercicio 4

cantidadCapasQueCumplen' :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen' f = pizzaProcesada (\i z -> fromEnum (f i) + z) 0

{- cantidadCapasQueCumplen' f = pizzaProcesada ((+) . fromEnum . f) 0
 -}
