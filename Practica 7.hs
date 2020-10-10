-- Ejercicio 1

data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa deriving Show


-- Regla base:
-- a) Prepizza esta en Pizza
-- Regla Inductiva
-- b) Si p esta en Pizza y i esta en ingredientes
-- entonces Capa i p está en el conjunto Pizza


-- Ejercicio 2
{- 
    f :: Pizza -> T
    f Prepizza = ...
    f (Capa i p) = ... (f p)
-}

-- :t Capa Anchoas (Capa Jamon (Capa Salsa Prepizza))

-- Ejercicio 3

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p

cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza = 0
cantidadDeAceitunas (Capa i p) = cantAceitunas i + (cantidadDeAceitunas p)

cantAceitunas :: Ingrediente -> Int
cantAceitunas (Aceitunas i) = i
cantAceitunas _ = 0

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i p) = Capa (dupAceitunas i) (duplicarAceitunas p)

dupAceitunas :: Ingrediente -> Ingrediente
dupAceitunas (Aceitunas i) = Aceitunas (2 * i)
dupAceitunas p = p


sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza = Prepizza
sinLactosa (Capa i p) = 
    if tieneLactosa i then
        sinLactosa p
    else 
        Capa i (sinLactosa p)

tieneLactosa :: Ingrediente -> Bool
tieneLactosa Queso = True
tieneLactosa _ = False


aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa Prepizza = True
aptaIntolerantesLactosa (Capa i p) = if (tieneLactosa i) then False else aptaIntolerantesLactosa p   


juntarAceitunas :: Ingrediente -> Pizza -> Pizza
juntarAceitunas (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n + m)) p
juntarAceitunas i p = Capa i p

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza = Prepizza
conDescripcionMejorada (Capa i p) = juntarAceitunas i (conDescripcionMejorada p)