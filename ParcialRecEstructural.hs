data Laberinto a = Salida
                 | Celda a
                 | Pasillo a (Laberinto a)
                 | Bifurcacion (Laberinto a) (Laberinto a)

haySalida :: Laberinto a -> Bool
haySalida Salida = True
haySalida (Celda _) = False
haySalida (Pasillo _ lab) = haySalida lab
haySalida (Bifurcacion lab1 lab2) = haySalida lab1 || haySalida lab2 


normSalidas :: Laberinto a -> Laberinto a
normSalidas Salida = Salida
normSalidas (Celda a) = Celda a
normSalidas (Pasillo n lab) = Pasillo n (normSalidas lab)
normSalidas (Bifurcacion Salida Salida) = Salida
normSalidas (Bifurcacion lab1 lab2) = Bifurcacion (normSalidas lab1) (normSalidas lab2)



haySalida = haySalida . normSalidas


PROP: haySalida = haySalida . normSalidas
DEM: por ppio de ext.
    ¿para todo lab :: Laberinto a. haySalida lab = (haySalida . normSalidas) lab?
Por definición de composición, es equivalente a
    ¿para todo lab :: Laberinto a. haySalida lab = haySalida (normSalidas lab)?

Caso Base.1 -- lab = Salida
    haySalida Salida = haySalida (normSalidas Salida)
    
    -- Lado Der
    haySalida (normSalidas Salida)
    -- = (normSalidas.1)
    haySalida Salida


    -- Queda demostrado Caso Base.1

Caso Base.2 -- lab = Celda n
    haySalida (Celda n) = haySalida (normSalidas (Celda n))
    -- Lado Der
    haySalida (normSalidas (Celda n))
    -- = (normSalidas.2)
    haySalida (Celda n)

    -- Queda demostrado Caso Base.2



Caso Inductivo.1 -- lab = Pasillo a lab1

    ¡HI: haySalida lab1 = haySalida (normSalidas lab1)!
    ¿TI: haySalida (Pasillo a lab1) = haySalida (normSalidas (Pasillo a lab1))?

    -- Lado Der
    haySalida (normSalidas (Pasillo a lab1))
    -- = (normSalidas.3)
    haySalida (Pasillo a (normSalidas lab1))
    -- = (haySalida.3)
    haySalida (normSalidas lab1)
    -- x HI
    haySalida lab1

    -- Lado Izq
    haySalida (Pasillo a lab1)
    -- = (haySalida.3)
    haySalida lab1

    -- Queda demostrado Caso Inductivo.1



Caso Inductivo.2 -- lab = Bifurcacion lab1 lab2

    ¡HI.1: haySalida lab1 = haySalida (normSalidas lab1)!
    ¡HI.2: haySalida lab2 = haySalida (normSalidas lab2)!
    ¿TI: haySalida (Bifurcacion lab1 lab2) = haySalida (normSalidas (Bifurcacion lab1 lab2))?

    -- Lado Der
    haySalida (normSalidas (Bifurcacion lab1 lab2))
    -- = (normSalidas.5)
    haySalida (Bifurcacion (normSalidas lab1) (normSalidas lab2))
    -- = (haySalida.4)
    haySalida (normSalidas lab1) || haySalida (normSalidas lab2)
    -- x HI.1 && HI.2
    haySalida lab1 || haySalida lab2

    -- Lado Izq
    haySalida (Bifurcacion lab1 lab2)
    -- = (haySalida.4)
    haySalida lab1 || haySalida lab2 

    -- Queda demostrado Caso Inductivo.2



normSalidas :: Laberinto a -> Laberinto a
normSalidas Salida = Salida
normSalidas (Celda a) = Celda a
normSalidas (Pasillo n lab) = Pasillo n (normSalidas lab)
normSalidas (Bifurcacion Salida Salida) = Salida
normSalidas (Bifurcacion lab1 lab2) = Bifurcacion (normSalidas lab1) (normSalidas lab2)

haySalida :: Laberinto a -> Bool
haySalida Salida = True
haySalida (Celda _) = False
haySalida (Pasillo _ lab) = haySalida lab
haySalida (Bifurcacion lab1 lab2) = haySalida lab1 || haySalida lab2 



-- Punto 3


data BoolExp = BBop BBO BoolExp BoolExp | BTrue | BFalse
data BBO = BAnd | BOr

bopToOP :: BBO -> Bool -> Bool -> Bool
bopToOP BAnd = (&&)
bopToOP BOr = (||)

evalBE :: BoolExp -> Bool
evalBE BTrue = True
evalBE BFalse = False
evalBE (BBop bop be1 be2) = (bopToOP bop) (evalBE be1) (evalBE be2)


haySalidaBE :: Laberinto a -> BoolExp
haySalidaBE Salida = BTrue
haySalidaBE (Celda _) = BFalse
haySalidaBE (Pasillo _ lab) = haySalidaBE lab
haySalidaBE (Bifurcacion lab1 lab2) = BBop BOr (haySalidaBE lab1) (haySalidaBE lab2)


-- Punto 4

evalBE . haySalidaBE = haySalida


PROP: evalBE . haySalidaBE = haySalida
DEM: por ppio de ext.
    ¿para todo lab :: Laberinto a. (evalBE . haySalidaBE) lab = haySalida lab?
Por definición de composición, es equivalente a
    ¿para todo lab :: Laberinto a. evalBE (haySalidaBE lab) = haySalida lab?


Caso Base.1 -- be = Salida
    evalBE (haySalidaBE Salida) = haySalida Salida
    -- Lado Izq
    evalBE (haySalidaBE Salida)
    -- = (haySalidaBE.1)
    evalBE BTrue
    -- = (evalBE.1)
    True

    -- Lado Der
    haySalida Salida
    -- = (haySalida.1)
    True

    -- Queda demostrado Caso Base.1


Caso Base.2 -- lab = Celda n
    evalBE (haySalidaBE (Celda n)) = haySalida (Celda n)
    -- Lado Izq
    evalBE (haySalidaBE (Celda n))
    -- = (haySalidaBE.2)
    evalBE BFalse
    -- = (evalBE.2)
    False

    -- Lado Der
    haySalida (Celda n)
    -- = (haySalida.2)
    False

    -- Queda demostrado Caso Base.2

Caso Inductivo.1 -- lab = Pasillo a lab1

    ¡HI: evalBE (haySalidaBE lab1) = haySalida lab1!
    ¿TI: evalBE (haySalidaBE (Pasillo a lab1)) = haySalida (Pasillo a lab1)?
    
    -- Lado Izq
    evalBE (haySalidaBE (Pasillo a lab1))
    -- = (haySalidaBE.3)
    evalBE (haySalidaBE lab1)
    -- x HI
    haySalida lab1

    -- Lado Der
    haySalida (Pasillo a lab1)
    -- = (haySalida.3)
    haySalida lab1


    -- Queda demostrado Caso Inductivo.1


Caso Inductivo.2 -- lab = Bifurcacion lab1 lab2

    ¡HI.1: evalBE (haySalidaBE lab1) = haySalida lab1!
    ¡HI.2: evalBE (haySalidaBE lab2) = haySalida lab2!
    ¿TI: evalBE (haySalidaBE (Bifurcacion lab1 lab2)) = haySalida (Bifurcacion lab1 lab2)?

    -- Lado Izq
    evalBE (haySalidaBE (Bifurcacion lab1 lab2))
    -- = (haySalidaBE.4)
    evalBE (BBop BOr (haySalidaBE lab1) (haySalidaBE lab2))
    -- = (evalBE.4)
    (bopToOP BOr) (evalBE (haySalidaBE lab1)) (evalBE (haySalidaBE lab2))
    -- = (bopToOP.1)
    (||) (evalBE (haySalidaBE lab1)) (evalBE (haySalidaBE lab2))
    -- x HI1 && HI2
    (||) (haySalida lab1) (haySalida lab2)
    -- = (def operador infijo ||)
    haySalida lab1 || haySalida lab2

    -- Lado Der
    haySalida (Bifurcacion lab1 lab2)
    -- = (haySalida.4)
    haySalida lab1 || haySalida lab2 

    -- Queda demostrado Caso Inductivo.2



bopToOP :: BBO -> Bool -> Bool -> Bool
bopToOP BAnd = (&&)
bopToOP BOr = (||)

evalBE :: BoolExp -> Bool
evalBE BTrue = True
evalBE BFalse = False
evalBE (BBop bop be1 be2) = (bopToOP bop) (evalBE be1) (evalBE be2)


haySalidaBE :: Laberinto a -> BoolExp
haySalidaBE Salida = BTrue
haySalidaBE (Celda _) = BFalse
haySalidaBE (Pasillo _ lab) = haySalidaBE lab
haySalidaBE (Bifurcacion lab1 lab2) = BBop BOr (haySalidaBE lab1) (haySalidaBE lab2)

haySalida :: Laberinto a -> Bool
haySalida Salida = True
haySalida (Celda _) = False
haySalida (Pasillo _ lab) = haySalida lab
haySalida (Bifurcacion lab1 lab2) = haySalida lab1 || haySalida lab2 


-- Punto 5

delta :: Bool -> Int
delta True  = 1
delta False = 0

cantQueCumplen :: (a -> Bool) -> Laberinto a -> Int
cantQueCumplen _ Salida = 0
cantQueCumplen p (Celda n) = delta (p n)
cantQueCumplen p (Pasillo n lab) = delta (p n) + cantQueCumplen p lab
cantQueCumplen p (Bifurcacion lab1 lab2) = cantQueCumplen p lab1 + cantQueCumplen p lab2

objs2Int :: (a -> Integer) -> Laberinto a -> Laberinto Integer
objs2Int _ Salida = Salida 
objs2Int f (Celda n) = Celda (f n) 
objs2Int f (Pasillo n lab) = Pasillo (f n) (objs2Int f lab)
objs2Int f (Bifurcacion lab1 lab2) = Bifurcacion (objs2Int f lab1) (objs2Int f lab2)

losQueCumplenPorDistancia :: (a -> Bool) -> Laberinto a -> [[a]]
losQueCumplenPorDistancia _ Salida = [[]]
losQueCumplenPorDistancia p (Celda n) = predicateToList p n
losQueCumplenPorDistancia p (Pasillo n lab) = (predicateToList p n) ++ losQueCumplenPorDistancia p lab
losQueCumplenPorDistancia p (Bifurcacion lab1 lab2) = juntarPorIndice (losQueCumplenPorDistancia p lab1) (losQueCumplenPorDistancia p lab2)

predicateToList :: (a -> Bool) -> a -> [[a]]
predicateToList p n = if (p n) then [[n]] else [[]]

juntarPorIndice :: [[a]] -> [[a]] -> [[a]]
juntarPorIndice (x:xs) (y:ys) = (x ++ y) : juntarPorIndice xs ys
juntarPorIndice l1 [] = l1
juntarPorIndice [] l2 = l2

esPar :: Integer -> Bool
esPar x = mod x 2 == 0

-- Punto 6

Suponiendo dadas las definiciones de los ejercicios anteriores, y la siguiente función

sumObj :: Laberinto Int -> Int
sumObj Salida                  = 0
sumObj (Celda n)               = n
sumObj (Pasillo x lab)         = x + sumObj lab
sumObj (Bifurcacion lab1 lab2) = sumObj lab1 + sumObj lab2

demostrar la siguiente propiedad 

       cantQueCumplen (const True) = sumObj . objs2Int (const 1)


data Laberinto a = Salida
                 | Celda a
                 | Pasillo a (Laberinto a)
                 | Bifurcacion (Laberinto a) (Laberinto a) deriving Show



PROP: cantQueCumplen (const True) = sumObj . objs2Int (const 1)
DEM: por ppio de ext.
    ¿para todo lab :: Laberinto a. cantQueCumplen (const True) lab = (sumObj . objs2Int (const 1)) lab?
Por definición de composición, es equivalente a
    ¿para todo lab :: Laberinto a. cantQueCumplen (const True) lab = sumObj (objs2Int (const 1) lab)?


Caso Base.1 -- lab = Salida
    cantQueCumplen (const True) Salida = sumObj (objs2Int (const 1) Salida)
    -- Lado Izq
    cantQueCumplen (const True) Salida
    -- = (cantQueCumplen.1)
    0

    -- Lado Der
    sumObj (objs2Int (const 1) Salida)
    -- = (objs2Int.1)
    sumObj Salida
    -- = (sumObj.1)
    0

        -- Queda demostrado caso Base 1

Caso Base.2 -- lab = Celda n
    cantQueCumplen (const True) (Celda n) = sumObj (objs2Int (const 1) (Celda n))
    -- Lado Izq
    cantQueCumplen (const True) (Celda n)
    -- = (cantQueCumplen.2)
    delta (const True (Celda n))
    -- = (const)
    delta True
    -- = (delta)
    1


    -- Lado Der
    sumObj (objs2Int (const 1) (Celda n))
    -- = (objs2Int.2)
    sumObj (Celda ((const 1) n))
    -- = (const)
    sumObj (Celda 1)
    -- = (sumobj.2)
    1

    -- Queda demostrado caso Base 2


-- Caso Inductivo.1 -- lab = Pasillo n lab1

    ¡HI: cantQueCumplen (const True) lab1 = sumObj (objs2Int (const 1) lab1)!
    ¿TI: cantQueCumplen (const True) (Pasillo n lab1) = sumObj (objs2Int (const 1) (Pasillo n lab1))?

    -- Lado Derecho
    cantQueCumplen (const True) (Pasillo n lab1)
    -- = (cantQueCumplen.3)
    delta ((const True) n) + cantQueCumplen (const True) lab1
    -- = (const)
    delta True + cantQueCumplen (const True) lab1
    -- = (delta.1)
    1 + cantQueCumplen (const True) lab1
    -- x HI
    1 + sumObj (objs2Int (const 1) lab1)

    -- Lado Izq
    sumObj (objs2Int (const 1) (Pasillo n lab1))
    -- = (objs2Int.3)
    sumObj (Pasillo ((const 1) n) (objs2Int (const 1) lab1))
    -- = (const) *2 
    sumObj (Pasillo 1 (objs2Int (const 1) lab1))
    -- = (sumObj.3)
    1 + sumObj (objs2Int (const 1) lab1)


    -- Queda demostrado caso ind.1


    -- FALTA CASO IND 2 POR FALTA DE TIEMPO :(

    Seria con -- lab = Bifurcacion lab1 lab2

    


delta :: Bool -> Int
delta True  = 1
delta False = 0

cantQueCumplen :: (a -> Bool) -> Laberinto a -> Int
cantQueCumplen _ Salida = 0
cantQueCumplen p (Celda n) = delta (p n)
cantQueCumplen p (Pasillo n lab) = delta (p n) + cantQueCumplen p lab
cantQueCumplen p (Bifurcacion lab1 lab2) = cantQueCumplen p lab1 + cantQueCumplen p lab2


sumObj :: Laberinto Int -> Int
sumObj Salida                  = 0
sumObj (Celda n)               = n
sumObj (Pasillo x lab)         = x + sumObj lab
sumObj (Bifurcacion lab1 lab2) = sumObj lab1 + sumObj lab2

objs2Int :: (a -> Integer) -> Laberinto a -> Laberinto Integer
objs2Int _ Salida = Salida 
objs2Int f (Celda n) = Celda (f n) 
objs2Int f (Pasillo n lab) = Pasillo (f n) (objs2Int f lab)
objs2Int f (Bifurcacion lab1 lab2) = Bifurcacion (objs2Int f lab1) (objs2Int f lab2)