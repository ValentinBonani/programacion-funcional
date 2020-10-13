-- SECCION 1
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

-- Ejercicio 4

cantidadDeAceitunas Prepizza = cantidadDeAceitunas (conDescripcionMejorada Prepizza)
-- Lado Der
cantidadDeAceitunas (conDescripcionMejorada Prepizza)
-- def de conDescMejorada
cantidadDeAceitunas Prepizza


cantidadDeAceitunas (Capa Queso Prepizza) = cantidadDeAceitunas (conDescripcionMejorada (Capa Queso Prepizza))
-- Lad Der
cantidadDeAceitunas (conDescripcionMejorada (Capa Queso Prepizza))
-- Def conDescripcionMejorada
cantidadDeAceitunas juntarAceitunas Queso (conDescripcionMejorada Prepizza)
-- Def conDescripcionMejorada
cantidadDeAceitunas juntarAceitunas Queso Prepizza
-- Def juntarAceitunas 
cantidadDeAceitunas (Capa Queso Prepizza)



cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza)) = cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza)))
-- Lad Der
cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza)))
-- Def conDescripcionMejorada
cantidadDeAceitunas juntarAceitunas (Aceitunas 8) (conDescripcionMejorada (Capa Queso Prepizza))
-- Def conDescripcionMejorada
cantidadDeAceitunas juntarAceitunas (Aceitunas 8) (juntarAceitunas Queso (conDescripcionMejorada Prepizza))
-- Def conDescripcionMejorada
cantidadDeAceitunas juntarAceitunas (Aceitunas 8) (juntarAceitunas Queso Prepizza)
-- Def juntarAceitunas de la derecha
cantidadDeAceitunas juntarAceitunas (Aceitunas 8) (Capa Queso Prepizza)
-- Def juntarAceitunas
cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))



cantidadDeAceitunas (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza))) =
cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza))))
-- Lado der
cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza))))
-- Def conDescripcionMejorada
cantidadDeAceitunas (juntarAceitunas (Aceitunas 9) (conDescripcionMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza))))
-- Def conDescripcionMejorada
cantidadDeAceitunas (juntarAceitunas (Aceitunas 9) (juntarAceitunas (Aceitunas 8) (conDescripcionMejorada (Capa Queso Prepizza))))
-- Def conDescripcionMejorada
cantidadDeAceitunas (juntarAceitunas (Aceitunas 9) (juntarAceitunas (Aceitunas 8) (juntarAceitunas Queso (conDescripcionMejorada Prepizza))))
-- Def conDescripcionMejorada
cantidadDeAceitunas (juntarAceitunas (Aceitunas 9) (juntarAceitunas (Aceitunas 8) (juntarAceitunas Queso Prepizza)))
-- Def juntarAceitunas de mas a la derecha
cantidadDeAceitunas (juntarAceitunas (Aceitunas 9) (juntarAceitunas (Aceitunas 8) (Capa Queso Prepizza)))
-- Def juntarAceitunas de mas a la derecha
cantidadDeAceitunas (juntarAceitunas (Aceitunas 9) (juntarAceitunas (Aceitunas 8) (Capa Queso Prepizza)))
-- Def juntarAceitunas de la derecha
cantidadDeAceitunas (juntarAceitunas (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza)))
-- Def juntarAceitunas
cantidadDeAceitunas ((Capa (Aceitunas 17) (Capa Queso Prepizza)))
-- Def cantidadDeAceitunas
cantAceitunas (Aceitunas 17) + (cantidadDeAceitunas (Capa Queso Prepizza))
-- Def cantidadDeAceitunas 
cantAceitunas (Aceitunas 17) + cantAceitunas Queso + (cantidadDeAceitunas Prepizza)
-- Def cantidadDeAceitunas 
cantAceitunas (Aceitunas 17) + cantAceitunas Queso + 0
-- Def cantAceitunas x2
17 + 0 + 0 
-- Por aritmetica simple
17

cantAceitunas i + (cantidadDeAceitunas p)
-- Lado izq
cantidadDeAceitunas (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza)))
-- Def cantidadDeAceitunas 
cantAceitunas (Aceitunas 9) + (cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza)))
-- Def cantidadDeAceitunas 
cantAceitunas (Aceitunas 9) + cantAceitunas (Aceitunas 8) + (cantidadDeAceitunas (Capa Queso Prepizza)))
-- Def cantidadDeAceitunas 
cantAceitunas (Aceitunas 9) + cantAceitunas (Aceitunas 8) + cantAceitunas Queso + (cantidadDeAceitunas Prepizza)
-- Def cantidadDeAceitunas 
cantAceitunas (Aceitunas 9) + cantAceitunas (Aceitunas 8) + cantAceitunas Queso + 0
-- Def cantAceitunas x3
9 + 8 + 0 + 0
-- Por aritmetica simple
17

-- SECION 2
type Nombre = String
data Planilla = Fin | Registro Nombre Planilla
data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo

-- Regla base:
-- a) Fin esta en Planilla
-- Regla Inductiva
-- b) Si p esta en Planilla y n esta en Nombre
-- entonces Registro n p está en el conjunto Planilla

f :: Plantilla -> T
f Fin = ...
f Registro n p = ... f p

-- Regla base:
-- a) Becario n esta en Equipo
-- Regla Inductiva
-- b) Si e1,e2,e3 estan en Equipo y n esta en Nombre
-- entonces Investigador n e1 e2 e3 está en el conjunto Equipo

f :: Equipo -> T
f Becario n = ...
f Investigador n e1 e2 e3 = ... f e1 ... f e2 ... f e3

-- Ejercicio 3

largoDePlanilla :: Planilla -> Int
largoDePlanilla Fin = 0
largoDePlanilla Registro _ p = 1 + largoDePlanilla p

esta :: Nombre -> Planilla -> Bool
esta nom Fin = False
esta nom Registro n p = if(nom == n) then True else (esta nom p)

juntarPlanillas :: Planilla -> Planilla -> Planilla
juntarPlanillas Fin Fin = Fin 
juntarPlanillas (Registro n p) Fin = Registro n p
juntarPlanillas Fin (Registro m p') = Registro m p'
juntarPlanillas (Registro n p) (Registro m p') = Registro n (Registro m (juntarPlanillas p p'))

nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario _) = 1
nivelesJerarquicos (Investigador n e1 e2 e3) = 1 + max (max (nivelesJerarquicos e1) (nivelesJerarquicos e2)) (nivelesJerarquicos e3)

cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario n) = 1
cantidadDeIntegrantes (Investigador _ e1 e2 e3) = 1 + cantidadDeIntegrantes e1 + cantidadDeIntegrantes e2 + cantidadDeIntegrantes e3

planillaDeIntegrantes :: Equipo -> Planilla
planillaDeIntegrantes (Becario n) = Registro n Fin
planillaDeIntegrantes (Investigador n e1 e2 e3) = Registro n juntarPlanillas ( juntarPlanillas (planillaDeIntegrantes e1) (planillaDeIntegrantes e2)) planillaDeIntegrantes e3

-- Ejercicio 4
-- 4a
largoDePlanilla (juntarPlanillas Fin p) = largoDePlanilla Fin + largoDePlanilla p
-- Dos casos 
-- caso p = Fin y caso p = Registro n p

-- caso p = Fin
-- Lado izq
largoDePlanilla (juntarPlanillas Fin Fin)
-- def juntarPlanillas 
largoDePlanilla Fin
-- def largoDePlanilla 
0
-- Lado der
largoDePlanilla Fin + largoDePlanilla Fin
-- def largoDePlanilla x 2
0 + 0
-- por aritmetica avanzada
0

-- caso p = Registro n p
-- Lado izq
largoDePlanilla (juntarPlanillas Fin (Registro n p))
-- def juntarPlanillas
largoDePlanilla (Registro m p) 

-- Lado der 
largoDePlanilla Fin + largoDePlanilla (Registro m p) 
-- def largo de planilla izq
0 + largoDePlanilla (Registro m p) 
-- aritmetica simple 
largoDePlanilla (Registro m p) 



-- 4b
largoDePlanilla (juntarPlanillas (Registro "Edsger" Fin) p) = largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla p
-- Dos casos 
-- caso p = Fin y caso p = Registro n p

-- caso p = Fin
-- Lado izq
largoDePlanilla (juntarPlanillas (Registro "Edsger" Fin) Fin)
-- Def juntarPlanillas
largoDePlanilla (Registro "Edsger" Fin)
-- Def largoDePlanilla 
1 + largoDePlanilla Fin
-- Def largoDePlanilla 
1 + 0
1

-- lado der
largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla Fin
-- def largoDePlanilla x2
1 + largoDePlanilla Fin + 0
-- def largoDePlanilla 
1 + 0 + 0
1

--caso p = Registro n p
-- Lado izq
largoDePlanilla (juntarPlanillas (Registro "Edsger" Fin) (Registro n p))
--def juntarPlanillas
largoDePlanilla (Registro "Edsger" (Registro m (juntarPlanillas Fin p)))
--def juntarPlanillas
largoDePlanilla (Registro "Edsger" (Registro m (juntarPlanillas Fin p)))




-- Ejercicio 5

a. demostrado en ejercicio c que para cualquier n son equivalentes ambas expresiones

b. demostrado en ejercicio c que para cualquier n son equivalentes ambas expresiones

c.
largoDePlanilla (planillaDeIntegrantes (Becario n)) = cantidadDeIntegrantes (Becario n)

-- Lado izq
largoDePlanilla (planillaDeIntegrantes (Becario n))
-- Def planillaDeIntegrantes
largoDePlanilla (Registro n Fin)
-- Def largoPlanilla
1 + largoDePlanilla Fin
-- Def largoDePlanilla
1 + 0
-- Aritmetica
1

-- Lado der 
cantidadDeIntegrantes (Becario n)
-- Def de cantidadDeIntegrantes
1

d. demostrado en ejercicio e que para cualquier n, n1, n2 y n3 son equivalentes ambas expresiones

e.
largoDePlanilla (planillaDeIntegrantes (Investigador n (Becario n1) (Becario n2) (Becario n3))) = cantidadDeIntegrantes (Investigador n (Becario n1) (Becario n2) (Becario n3))
-- Lado Derecho
largoDePlanilla (planillaDeIntegrantes (Investigador n (Becario n1) (Becario n2) (Becario n3)))
-- Def planillaDeIntegrantes
largoDePlanilla (Registro n juntarPlanillas (juntarPlanillas (planillaDeIntegrantes (Becario n1)) (planillaDeIntegrantes (Becario n2))) planillaDeIntegrantes (Becario n3))
-- Def planillaDeIntegrantes x 3
largoDePlanilla (Registro n juntarPlanillas (juntarPlanillas (Registro n1 Fin) (Registro n2 Fin)) (Registro n3 Fin))
-- Def juntarPlantillas de la der
largoDePlanilla (Registro n juntarPlanillas (Registro n1 (Registro n2 (juntarPlanillas Fin Fin))) (Registro n3 Fin))
-- Def juntarPlantillas de la der
largoDePlanilla (Registro n juntarPlanillas (Registro n1 (Registro n2 Fin)) (Registro n3 Fin))
-- Def juntarPlanillas
largoDePlanilla (Registro n (Registro n1 (Registro n3 (Registro n2 Fin))))
-- Def largoDePlanilla
1 + largoDePlanilla (Registro n1 (Registro n3 (Registro n2 Fin)))
-- Def largoDePlanilla
1 + 1 + largoDePlanilla (Registro n3 (Registro n2 Fin))
-- Def largoDePlanilla
1 + 1 + 1 +  largoDePlanilla  (Registro n2 Fin)
-- Def largoDePlanilla
1 + 1 + 1 + 1 + largoDePlanilla Fin
-- Def largoDePlanilla
1 + 1 + 1 + 1 + 0
-- aritmetica
4

--lado izq
cantidadDeIntegrantes (Investigador n (Becario n1) (Becario n2) (Becario n3))
-- def cantidadDeIntegrantes
1 + cantidadDeIntegrantes (Becario n1) + cantidadDeIntegrantes (Becario n2) + cantidadDeIntegrantes (Becario n3)
-- def cantidadDeIntegrantes x3 
1 + 1 + 1 + 1
-- aritmetica
4


--Secion 3

data Dungeon a = Habitacion a | Pasaje (Maybe a) (Dungeon a) | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a)

-- Regla base:
-- a) Habitacion a esta en Dungeon
-- Regla Inductiva
-- b) si (Maybe a) (Dungeon a) esta en Dungeon
-- entonces Pasaje (Maybe a) (Dungeon a) está en el conjunto Dungeon
-- c) si (Maybe a) (Dungeon a) (Dungeon a) esta en Dungeon
-- entonces Bifurcacion (Maybe a) (Dungeon a) (Dungeon a) está en el conjunto Dungeon

f :: Dungeon -> T
f (Habitacion a) = ...
f (Pasaje m d) = ... f d
f (Bifurcacion m d1 d2) = ... f d1 ... f d2

-- Ejercicio 3

-- data Maybe a = Nothing | Just a

cantidadDeBifurcaciones :: Dungeon -> Int
cantidadDeBifurcaciones (Habitacion a) = 0
cantidadDeBifurcaciones (Pasaje m d) = cantidadDeBifurcaciones d
cantidadDeBifurcaciones (Bifurcacion m d1 d2) = 1 + cantidadDeBifurcaciones d1 + cantidadDeBifurcaciones d2

hayElemento :: Maybe a -> Int
hayElemento Just _ = 1
hayElemento Nothing = 0

noHayElemento :: Maybe a -> Int
hayElemento Just _ = 0
hayElemento Nothing = 1

cantidadDePuntosInteresantes :: Dungeon -> Int
cantidadDePuntosInteresantes (Habitacion a) = 1
cantidadDePuntosInteresantes (Pasaje m d) = hayElemento m + cantidadDePuntosInteresantes d
cantidadDePuntosInteresantes (Bifurcacion m d1 d2) = hayElemento m + cantidadDePuntosInteresantes d1 + cantidadDePuntosInteresantes d2

cantidadDePuntosVacios :: Dungeon -> Int
cantidadDePuntosVacios (Habitacion a) = 0
cantidadDePuntosVacios (Pasaje m d) = noHayElemento m + cantidadDePuntosVacios d
cantidadDePuntosVacios (Bifurcacion m d1 d2) = noHayElemento m + cantidadDePuntosVacios d1 + cantidadDePuntosVacios d2

genericF :: (Maybe a -> Int) -> Dungeon -> Int
genericF f (Habitacion a) = f a
genericF f (Pasaje m d) = f m + cantidadDePuntosInteresantes d
genericF _ (Bifurcacion m d1 d2) = f m + cantidadDePuntosInteresantes d1 + cantidadDePuntosInteresantes d2


tieneElemento :: (Eq a) => a -> Maybe a -> Int
tieneElemento e (Just e') = if (e == e') then 1 else 0
tieneElemento e Nothing = 0

cantidadDePuntosCon :: a -> Dungeon -> Int
cantidadDePuntosCon e (Habitacion a) = tieneElemento e (Just a)
cantidadDePuntosCon e (Pasaje m d) = (tieneElemento e m) + cantidadDePuntosCon d
cantidadDePuntosCon e (Bifurcacion m d1 d2) = (tieneElemento e m) + cantidadDePuntosCon d1 + cantidadDePuntosCon d2

esLineal :: Dungeon -> Bool
esLineal (Habitacion a) = True
esLineal (Pasaje _ d) = esLineal d
esLineal (Bifurcacion _ _ _) = False

llenoDe :: a -> Dungeon -> Bool
llenoDe e (Habitacion a) = True
llenoDe e (Pasaje m d) = if((tieneElemento e) == 1) then llenoDe d else False
llenoDe e (Bifurcacion m d1 d2) = if((tieneElemento e) == 1) then (llenoDe d1) && (llenoDe d2) else False

-- Ejercicio 4
data Tesoro = Cofre | Oro | Joyas
a.  cantidadDePuntosVacios (Habitacion x) = 0
-- def de cantidadDePuntosVacios
noHayElemento x


b.  cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas)) = 1
-- def de cantidadDePuntosVacios
noHayElemento Nothing + cantidadDePuntosVacios (Habitacion Joyas)
-- def de cantidadDePuntosVacios
noHayElemento Nothing + 0
-- def noHayElemento
1 + 0
-- aritmetica
1

c. cantidadDePuntosVacios (Pasaje (Just y) (Habitacion x)) = 0
-- Def cantidadDePuntosVacios
noHayElemento (Just y) + cantidadDePuntosVacios (Habitacion x)
-- Def cantidadDePuntosVacios
noHayElemento (Just y) + 0
-- Def noHayElemento
0 + 0
-- aritmetica
0

d. demostrado en ejercicio e que para cualquier z,y,x son equivalentes ambas expresiones

e. cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing (Habitacion z)) (Pasaje (Just y) (Habitacion x))) = 2
-- Def  cantidadDePuntosVacios
 noHayElemento Nothing + cantidadDePuntosVacios (Pasaje Nothing (Habitacion z)) + cantidadDePuntosVacios (Pasaje (Just y) (Habitacion x))
-- Def  cantidadDePuntosVacios x2
noHayElemento Nothing + noHayElemento Nothing + cantidadDePuntosVacios (Habitacion z) + noHayElemento (Just y) +  cantidadDePuntosVacios (Habitacion x)
-- Def  cantidadDePuntosVacios x2
noHayElemento Nothing + noHayElemento Nothing + 0 + noHayElemento (Just y) + 0
-- Def  noHayElemento x3
1 + 1 + 0 + 0 + 0
-- aritmetica
2

-- Ejercicio 5
data VariasCosas a b = Objeto a | Criatura b
data Monstruo = Gargola | Dragon | Troll

a. cantidadDePuntosCon (Criatura Troll) (Habitacion (Objeto Oro)) = 0
-- def cantidadDePuntosCon
tieneElemento (Criatura Troll) (Just (Objeto Oro))
-- def tieneElemento
if ((Criatura Troll) == (Objeto Oro)) then 1 else 0
-- def ==
if (False) then 1 else 0
-- def if
    


b. cantidadDePuntosCon (Criatura Troll) (Pasaje (Just (Criatura Troll)) (Habitacion (Objeto Oro)))) = 1
-- def cantidadDePuntosCon
(tieneElemento (Criatura Troll) (Just (Criatura Troll))) + cantidadDePuntosCon (Habitacion (Objeto Oro))
-- def cantidadDePuntosCon
(tieneElemento (Criatura Troll) (Just (Criatura Troll))) + 0
-- def tieneElemento
1 + 0
-- aritmetica
1


c. cantidadDePuntosCon (Criatura Troll) (Pasaje (Just (Criatura Troll)) (Habitacion (Criatura Troll)))) = 2
-- def cantidadDePuntosCon
(tieneElemento (Criatura Troll) (Just (Criatura Troll))) + cantidadDePuntosCon (Habitacion (Criatura Troll))
-- def cantidadDePuntosCon
(tieneElemento (Criatura Troll) (Just (Criatura Troll))) + 1
-- def tieneElemento
1 + 1
-- aritmetica
2

cantidadDePuntosCon :: a -> Dungeon -> Int
cantidadDePuntosCon _ (Habitacion a) = 0
cantidadDePuntosCon e (Pasaje m d) = (tieneElemento e m) + cantidadDePuntosCon d
cantidadDePuntosCon e (Bifurcacion m d1 d2) = (tieneElemento e m) + cantidadDePuntosCon d1 + cantidadDePuntosCon d2