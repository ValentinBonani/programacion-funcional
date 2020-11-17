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

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas _) = True
esAceituna _ = False


 
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

conCapasTransformadas' :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas' ingTransformer = pizzaProcesada (Capa . ingTransformer) Prepizza

soloLasCapasQue' :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue' matchPredicate = pizzaProcesada (\i z -> if matchPredicate i then Capa i z else z) Prepizza

sinLactosa' :: Pizza -> Pizza
sinLactosa' = pizzaProcesada (\i z -> if (not . esQueso) i then Capa i z else z) Prepizza

aptaIntolerantesLactosa' :: Pizza -> Bool
aptaIntolerantesLactosa' = pizzaProcesada (\i z -> (not . esQueso) i && z) True

cantidadDeQueso' :: Pizza -> Int
cantidadDeQueso' = pizzaProcesada ((+) . fromEnum . esQueso) 0

conElDobleDeAceitunas' :: Pizza -> Pizza
conElDobleDeAceitunas' = pizzaProcesada (Capa . aceitunaDuplicator) Prepizza

-- Ejercicio 5

cantidadAceitunas :: Pizza -> Int
cantidadAceitunas = pizzaProcesada ((+) . fromEnum . esAceituna) 0

capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen p = pizzaProcesada (\i z -> if p i then i : z else z) []

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada = pizzaProcesada juntarAceitunas Prepizza

juntarAceitunas :: Ingrediente -> Pizza -> Pizza
juntarAceitunas (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n + m)) p
juntarAceitunas i p = Capa i p

tieneAceitunaAdelante :: Pizza -> Bool
tieneAceitunaAdelante (Capa (Aceitunas _) _) = True
tieneAceitunaAdelante _ = False

getAceitunas :: Pizza -> Int
getAceitunas (Capa (Aceitunas n) _) = n

conCapasDe :: Pizza -> Pizza -> Pizza
conCapasDe = pizzaProcesada (\i z -> Capa i z) 