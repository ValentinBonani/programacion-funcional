-- Ejercicio 1
doble :: Int -> Int
doble x = x + x
cuadruple x = 4 * x
cuatro = (\x -> 4)
cuatro' = (\x -> 2 + 2)
cuatro'' x = doble 2  
cuatro''' x = doble 1 + 2
cuatro'''' x = doble (doble 1)
cuatro''''' x = cuadruple 1
cuatro'''''' = -1 + 5
cuatro''''''' = -4 * (-1)

-- Ejercicio 2

{- 
    doble (doble 2)  
        --def. de doble, con x <- 2
    doble (2 + 2)
        --def. suma
    doble (4)
        --def. de doble, con x <- 4
    (4 + 4)
        --def. suma
    8 
-}

{- 
    doble (doble 2)
        --def. de doble, con x <- (doble 2)
    (doble 2) + (doble 2)
        --def. de doble, con x <- 2
    (2 + 2) + (2 + 2)
        --def. suma
    8 
-}

-- Ejercicio 3

{- 
    cuadruple 2
        -- def. de cuadruple, con x <- 2
    (4 * 2)
        -- def. de mult
    8 
-}
{- 
    cuadruple (cuadruple 2)
        -- def. de cuadruple, con x <- 2
    cuadruple (4 * 2) -- otro camino seria ir por def de cuadruple con (x <- 4 * 2)
        -- def. de mult
    cuadruple (8)
        -- def. de cuadruple, con x <- 8
    4 * 8
        -- def. de mult
    32 
-}

{- 
    cuadruple (cuadruple 2)
        -- def. de cuadruple, con x <- cuadruple 2
    4 * (cuadruple 2)
        -- def. de cuadruple, con x <- 2
    4 * (4 * 2)
        -- def. de mult
    32 
-}

-- Ejercicio 4

triple x = 3 * x
succ' x = x + 1
sumarDos x = x + 2

-- Ejercicio 5

twice f = g
    where g x = f (f x)

-- Esta expresion es verdadera
{- twice succ 1 == sumarDos 1 -}

-- Ejercicio 6

{- 
    doble == \x -> x + x
    doble' x = x * 2

    doble == doble'
    twice doble 2 == doble (doble 2)
    doble (doble 2) == cuadruple 2 
-}

-- Ejercicio 7

 {- twice f = g
    where g x = f (f x) -}

{-
    ((twice twice) doble) 3
        -- def. de twice, con f <- twice
    (g doble) 3
        -- donde g x = twice (twice x)
        -- def. de g con x <- doble
    twice (twice doble) 3
        -- def. de twice, con f <- (twice doble)
    g' 3
        -- donde g' x = (twice doble) ((twice doble) x)
        -- def. de g' con x <- 3
    (twice doble) ((twice doble) 3)
        -- def. de twice, con f <- doble
    (twice doble) (g'' 3)
        -- donde g'' x = doble (doble x)
        -- def. de g'' con x <- 3
    (twice doble) (doble (doble 3))
        -- def. de twice, con f <- doble
    g''' (doble (doble 3))
        -- donde g''' x = doble (doble x)
        -- def. de g''' con x <- (doble (doble 3)) 
    doble (doble (doble (doble 3)))
        -- def de doble con x <- 3
    doble (doble (doble (3 + 3)))
        -- def de suma
    doble (doble (doble (6)))
        -- def de doble con x <- 6
    doble (doble (6 +6))
        -- def de suma
    doble (doble (12))
        -- def de doble con x <- 12
    doble(12 + 12)
        -- def de suma
    doble 24
        -- def de doble con x <- 24
    24 + 24
        -- def de suma
    48
 -}