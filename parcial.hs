data Laberinto a = Salida
                 | Celda a
                 | Pasillo a (Laberinto a)
                 | Bifurcacion (Laberinto a) (Laberinto a)


haySalida :: Laberinto a -> Bool
haySalida Salida = True
haySalida (Celda a) = False
haySalida (Pasillo a l) = haySalida l
haySalida (Bifurcacion l1 l2) = haySalida l1 || haySalida l2

normSalidas :: Laberinto a -> Laberinto a
normSalidas Salida = Salida
normSalidas (Celda a) = Celda a
normSalidas (Pasillo a l) = Pasillo a (normSalidas l)
normSalidas (Bifurcacion Salida Salida) = Salida
normSalidas (Bifurcacion l1 l2) = Bifurcacion (normSalidas l1) (normSalidas l2)


haySalida = haySalida . normSalidas

Proposición: haySalida = haySalida . normSalidas
Demostración por principio de extensionalidad
    ¿para todo l :: Laberinto a. haySalida l = (haySalida . normSalidas) l?
Por definición de la composición, es equivalente a
    ¿para todo l :: Laberinto a. haySalida l = haySalida (normSalidas l)? 

Casos bases:
    Caso base 1 con l = Salida:

    haySalida Salida = haySalida (normSalidas Salida)
    -- lado derecho
        haySalida (normSalidas Salida)
    -- = normSalidas.1
        haySalida Salida
    -- igual que el lado izquierdo

    Caso base 2 con l = Celda a:

    haySalida (Celda a) = haySalida (normSalidas (Celda a))
    -- lado derecho
        haySalida (normSalidas (Celda a))
    -- = normSalidas.2
        haySalida (Celda a)
    -- igual que el lado izquierdo

Casos inductivos:

    Caso inductivo 1 con l = Pasillo a l'

        ¡HI: haySalida l' = haySalida (normSalidas l')! 
        ¿TI: haySalida (Pasillo a l') = haySalida (normSalidas (Pasillo a l'))?

    -- Lado derecho
        haySalida (normSalidas (Pasillo a l'))
    -- = normSalidas.3
        haySalida (Pasillo a (normSalidas l'))
    -- = haySalida.3
        haySalida (normSalidas l')
    -- por HI
        haySalida l'
    
    -- lado izquierdo
        haySalida (Pasillo a l')
    -- = haySalida.3
        haySalida l'
    -- igual que el lado derecho


    Caso inductivo 2 con l = Bifurcacion l1 l2

        ¡HI.1: haySalida l1 = haySalida (normSalidas l1)! 
        ¡HI.2: haySalida l2 = haySalida (normSalidas l2)! 
        ¿TI: haySalida (Bifurcacion l1 l2) = haySalida (normSalidas (Bifurcacion l1 l2))?

    -- Lado derecho
        haySalida (normSalidas (Bifurcacion l1 l2))
    -- = normSalidas.5
        haySalida (Bifurcacion (normSalidas l1) (normSalidas l2))
    -- = haySalida.4
        haySalida (normSalidas l1) || haySalida (normSalidas l2)
    -- Por HI.1 y HI.2
        haySalida l1 || haySalida l2

    -- Lado izquierdo
        haySalida (Bifurcacion l1 l2)
    -- = haySalida.4
        haySalida l1 || haySalida l2
    -- igual que el lado derecho

    
-----------------------------------------------------------------------------------------
EJ2
Dado el siguiente tipo algebraico (y también el tipo Laberinto de los ejercicios anteriores)

data BoolExp = BBop BBO BoolExp BoolExp | BTrue | BFalse
data BBO = BAnd | BOr

definir las siguientes funciones

evalBE :: BoolExp -> Bool, que dada una expresión booleana da su significado, como el booleano resultante de evaluarla
haySalidaBE :: Laberinto a -> BoolExp, que dado un laberinto, construye una expresión booleana que expresa que el laberinto tiene salida, de manera puramente simbólica.
Como recordatorio, el tipo Laberinto se define como

data Laberinto a = Salida
                 | Celda a
                 | Pasillo a (Laberinto a)
                 | Bifurcacion (Laberinto a) (Laberinto a)


evalBE :: BoolExp -> Bool,
evalBE BTrue = True
evalBE BFalse = False
evalBE (BBop BAnd b1 b2) = evalBE b1 && evalBE b2
evalBE (BBop BOr b1 b2) = evalBE b1 || evalBE b2

haySalidaBE :: Laberinto a -> BoolExp
haySalidaBE Salida = BTrue
haySalidaBE (Celda a) = BFalse
haySalidaBE (Pasillo a l) = haySalidaBE l
haySalidaBE (Bifurcacion l1 l2) = BBop BOr (haySalidaBE l1) (haySalidaBE l2)


evalBE . haySalidaBE = haySalida


Proposición: evalBE . haySalidaBE = haySalida
Demostración por principio de extensionalidad
    ¿para todo l :: Laberinto a. (evalBE . haySalidaBE) l = haySalida l?
Por definición de la composición, es equivalente a
    ¿para todo l :: Laberinto a. evalBE (haySalidaBE l) = haySalida l? 

Casos bases:
    Caso base 1 con l = Salida:
        
        evalBE (haySalidaBE Salida) = haySalida Salida
        -- lado izquierdo
            evalBE (haySalidaBE Salida)
        -- = haySalidaBE.1
            evalBE BTrue
        -- = evalBE.1
            True
        
        -- lado derecho
            haySalida Salida
        -- = haySalida.1
            True
        -- igual que el lado izquierdo


    Caso base 2 con l = Celda a:

        evalBE (haySalidaBE (Celda a)) = haySalida (Celda a)
        -- Lado izquierdo
            evalBE (haySalidaBE (Celda a))
        -- = haySalidaBE.2
            evalBE BFalse
        -- = evalBE.2
            False

        -- lado derecho
            haySalida (Celda a)
        -- = haySalida.2
            False
        -- igual que el lado izquierdo

    
Casos inductivos:

    Caso inductivo 1 con l = Pasillo a l'

        ¡HI: evalBE (haySalidaBE l') = haySalida l'! 
        ¿TI: evalBE (haySalidaBE (Pasillo a l')) = haySalida (Pasillo a l')?

        -- Lado izquierdo
            evalBE (haySalidaBE (Pasillo a l'))
        -- = haySalidaBE.3
            evalBE (haySalidaBE l')
        -- por HI
            haySalida l'

        -- Lado derecho
            haySalida (Pasillo a l')
        -- = haySalida.3
            haySalida l'
        -- igual que el lado izquierdo
        

    Caso inductivo 2 con l = Bifurcacion l1 l2

        ¡HI.1: evalBE (haySalidaBE l1) = haySalida l1! 
        ¡HI.2: evalBE (haySalidaBE l2) = haySalida l2! 
        ¿TI: evalBE (haySalidaBE (Bifurcacion l1 l2)) = haySalida (Bifurcacion l1 l2)?

        -- Lado izquierdo
            evalBE (haySalidaBE (Bifurcacion l1 l2))
        -- = haySalidaBE.4
            evalBE (BBop BOr (haySalidaBE l1) (haySalidaBE l2))
        -- = evalBE.4  
            evalBE (haySalidaBE l1) || evalBE (haySalidaBE l2)
        -- Por HI.1 y HI.2
            haySalida l1 || haySalida l2
        
        -- Lado derecho
            haySalida (Bifurcacion l1 l2)
        -- = haySalida.4
            haySalida l1 || haySalida l2
        -- igual que el lado izquierdo


------------------------------------------------------

Suponiendo dadas las definiciones de los ejercicios anteriores, y dada la siguiente función auxiliar

delta :: Bool -> Int
delta True  = 1
delta False = 0

definir las siguientes funciones

    cantQueCumplen :: (a -> Bool) -> Laberinto a -> Int, que dado un predicado sobre elementos de tipo a y un laberinto, describe cuántos de los elementos que se encuentran en el laberinto dado cumplen el predicado dado.
    objs2Int :: (a -> Int) -> Laberinto a -> Laberinto Int, que dada una función de peso sobre elementos de tipo a y un laberinto, describe un laberinto donde todos los elementos fueron reemplazados por su peso.
    losQueCumplenPorDistancia :: (a -> Bool) -> Laberinto a -> [[a]], que dado un predicado sobre los elementos de tipo a y un laberinto, retorna la lista de aquellas listas que contienen a todos los elementos que aparecen en el laberinto a la misma distancia de la entrada (sin contar las bifurcaciones).
    Por ejemplo,
    losQueCumplenPorDistancia esPar
          (Pasillo 2 (Bifurcación (Pasillo 6 (Bifurcación (Celda 8) (Celda 4))) (Pasillo 10 (Celda 12))))
      = [[2], [6,10], [8,4,12]]
    porque 2 está a distancia 0 de la entrada, 6 y 10 están a distancia 1 de la entrada, y 8, 4 y 12 están a distancia 2 de la entrada.

Como recordatorio, el tipo Laberinto se define como

data Laberinto a = Salida                 | Celda a                 | Pasillo a (Laberinto a)                 | Bifurcacion (Laberinto a) (Laberinto a) 

-------------------------------------------------------------------------


cantQueCumplen :: (a -> Bool) -> Laberinto a -> Int
cantQueCumplen f Salida = 0
cantQueCumplen f (Celda a) = delta (f a)
cantQueCumplen f (Pasillo a l) = delta (f a) + cantQueCumplen f l
cantQueCumplen f (Bifurcacion l1 l2) = cantQueCumplen f l1 + cantQueCumplen f l2


objs2Int :: (a -> Int) -> Laberinto a -> Laberinto Int
objs2Int f Salida = Salida
objs2Int f (Celda a) = Celda (f a)
objs2Int f (Pasillo a l) = Pasillo (f a) (objs2Int f l)
objs2Int f (Bifurcacion l1 l2) = Bifurcacion (objs2Int f l1) (objs2Int f l2)

evalFA :: (a -> Bool) -> a -> [[a]]
evalFA f a = if (f a) then [[a]] else [[]]

mergeByLevel :: [[a]] -> [[a]] -> [[a]]
mergeByLevel l1 [] = l1
mergeByLevel [] l2 = l2
mergeByLevel (x:xs) (y:ys) = (x++y) : mergeByLevel xs ys

losQueCumplenPorDistancia :: (a -> Bool) -> Laberinto a -> [[a]]
losQueCumplenPorDistancia f Salida = [[]]
losQueCumplenPorDistancia f (Celda a) = evalFA f a
losQueCumplenPorDistancia f (Pasillo a l) = evalFA f a ++ (losQueCumplenPorDistancia f l)
losQueCumplenPorDistancia f (Bifurcacion l1 l2) = mergeByLevel (losQueCumplenPorDistancia f l1) (losQueCumplenPorDistancia f l2)



-----------------------------

ids = (sumObj . objs2Int (const 1)) lab

Suponiendo dadas las definiciones de los ejercicios anteriores, y la siguiente función

sumObj :: Laberinto Int -> Int
sumObj Salida = 0
sumObj (Celda n) = n
sumObj (Pasillo x lab) = x + sumObj lab
sumObj (Bifurcacion lab1 lab2) = sumObj lab1 + sumObj lab2

demostrar la siguiente propiedad 

       cantQueCumplen (const True) = sumObj . objs2Int (const 1)


Proposición: cantQueCumplen (const True) = sumObj . objs2Int (const 1)
Demostración por principio de extensionalidad
    ¿para todo l :: Laberinto a. cantQueCumplen (const True) l = (sumObj . objs2Int (const 1)) l?
Por definición de la composición, es equivalente a
    ¿para todo l :: Laberinto a. cantQueCumplen (const True) l = sumObj (objs2Int (const 1) l)? 

Casos bases:
    Caso base 1 con l = Salida:

        cantQueCumplen (const True) Salida = sumObj (objs2Int (const 1) Salida)

        -- Lado izquierdo
            cantQueCumplen (const True) Salida
        -- = cantQueCumplen.1
            0

        -- Lado derecho
            sumObj (objs2Int (const 1) Salida)
        -- = objs2Int.1
            sumObj Salida
        -- = sumObj.1
            0
        -- Lo mismo que el lado izquierdo


    Caso base 2 con l = Celda n:

        cantQueCumplen (const True) (Celda n) = sumObj (objs2Int (const 1) (Celda n))

        -- Lado izquierdo
            cantQueCumplen (const True) (Celda n)
        -- = cantQueCumplen.2
            delta ((const True) n)
        -- = const
            delta True
        -- = delta
            1

        -- Lado derecho
            sumObj (objs2Int (const 1) (Celda n))
        -- = objs2Int.2
            sumObj (Celda ((const 1) n))
        -- = sumObj.2
            (const 1) n
        -- = const
            1

Casos inductivos:
    Caso inductivo 1 con l = Pasillo a l'

    ¡HI: cantQueCumplen (const True) l' = sumObj (objs2Int (const 1) l')! 
    ¿TI: cantQueCumplen (const True) (Pasillo a l') = sumObj (objs2Int (const 1) (Pasillo a l'))?

    -- Lado izquierdo
        cantQueCumplen (const True) (Pasillo a l')
    -- = cantQueCumplen.3
        delta ((const True) a) + cantQueCumplen (const True) l'
    -- Por HI
        delta ((const True) a) + sumObj (objs2Int (const 1) l')
    -- = const
        delta (True) + sumObj (objs2Int (const 1) l')
    -- = delta
        1 +sumObj (objs2Int (const 1) l')

    -- Lado derecho
        sumObj (objs2Int (const 1) (Pasillo a l'))
    -- = objs2Int.3
        sumObj (Pasillo ((const 1) a) (objs2Int (const 1) l'))
    -- = const
        sumObj (Pasillo 1 (objs2Int (const 1) l'))
    -- = sumObj.3
        1 + sumObj (objs2Int (const 1) l')
    -- Lo mismo que el lado izquierdo


    Caso inductivo 2 con l = Bifurcacion l1 l2

        ¡HI.1: cantQueCumplen (const True) l1 = sumObj (objs2Int (const 1) l1)! 
        ¡HI.2: cantQueCumplen (const True) l2 = sumObj (objs2Int (const 1) l2)! 
        ¿TI: cantQueCumplen (const True) (Bifurcacion l1 l2) = sumObj (objs2Int (const 1) (Bifurcacion l1 l2))?

        -- Lado izquierdo
            cantQueCumplen (const True) (Bifurcacion l1 l2)
        -- = cantQueCumplen.4
            cantQueCumplen (const True) l1 + cantQueCumplen (const True) l2
        -- Por HI.1 y HI.2
            sumObj (objs2Int (const 1) l1) + sumObj (objs2Int (const 1) l2)

        -- Lado derecho
            sumObj (objs2Int (const 1) (Bifurcacion l1 l2))
        -- = objs2Int.4
            sumObj (Bifurcacion (objs2Int (const 1) l1) (objs2Int (const 1) l2))
        -- = sumObj.4
            sumObj (objs2Int (const 1) l1) + sumObj (objs2Int (const 1) l2)
        -- Lo mismo que el lado iquierdo


