-- Ejercicio 1

-- ADICIONAL
swap :: (b, a) -> (a, b)
swap (x, y) = (y, x)
-- FIN ADICIONAL

duplicarTupla = \(x,y) -> (x*2,y*2)

curry' :: ((a,b) -> c) -> a -> b -> c
curry' = \f -> \a -> \b -> f (a,b) 

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' = \f -> \(a,b) -> f a b

duplicarTuplaCurry :: Int -> Int -> (Int, Int)
duplicarTuplaCurry = curry duplicarTupla

duplicarTuplaCurryUncurry :: (Int, Int) -> (Int, Int)
duplicarTuplaCurryUncurry = uncurry (curry duplicarTupla)

e1R :: Bool
e1R = duplicarTuplaCurryUncurry (2,3) == duplicarTupla (2,3)

e1R' :: Bool
e1R' = curry' (uncurry' (+)) 1 2 == (+) 1 2

-- Ejercicio 3

apply :: a -> a
twice :: (a -> a) -> a -> a 
id' :: a -> a
flip' :: (a -> b -> c) -> b -> a -> c
uflip :: ((a,b) -> c) -> (b,a) -> c
const :: a -> b -> a
compose :: (a -> b) -> (c -> a) -> c -> b

-- Ejercicio 2

apply f = f

twice f = f . f

id' x = x

flip' f x y = f y x

uflip f = f . swap

const x y = x

compose = (.)

-- Ejercicio 4

doble :: Num a => a -> a
doble x = x + x

e41 :: a -> a
e41 = (apply apply) apply

e42 :: Int
e42 = (twice doble) 2

{- e43 :: (a,b) -> (a,b) -}
e43 = ((twice twice) twice) swap

e44 :: Int
e44 = ((flip twice) 1) doble


-- Ejercicio 5

appDup f = g
    where g x = f (x, x)

appDup' = \f -> \x -> f (x,x)

appFork (f, g) = h
    where h x = (f x, g x)

appFork' = \(f,g) -> \x -> (f x, g x)

appPar (f, g) = h
    where h (x, y) = (f x, g y)

appPar' = \(f,g) -> \(x,y) -> (f x, g y)

appDist f = g
    where g (x, y) = (f x, f y)

appDist' f = \f -> \(x,y) -> (f x, f y)

{- subst f = h
    where h g = k;
        k x = (f x)  (g x)} -}
subst = \f -> \g -> \x -> f x (g x)


-- Ejercicio 6

-- compose (fst snd) NO ANDA 
e6a :: (a, (b1, b2)) -> b1
e6a = compose fst snd

-- (uncurry curry snd) NO ANDA
e6b :: (a, c) -> c
e6b = uncurry (curry snd)

-- (apply id) ((id apply) apply) FUNCIONA OK
e6c :: a -> a
e6c = (apply id) ((id apply) apply)

-- compose (compose doble doble) FUNCIONA OK
e6d :: (c -> Int) -> c -> Int
e6d = compose (compose doble doble)

-- (compose compose) doble doble NO ANDA
e6e :: (c -> Int) -> c -> Int
e6e = compose (compose doble doble)

-- Ejercicio 7 
many :: Int -> (a -> a) -> a -> a
many 0 f x = x
many n f x = f (many (n-1) f x)

{- 
    many 3 doble 2
    doble (many (2) doble 2)
    doble (doble (many (1) doble 2))
    doble (doble (doble (many (0) doble 2))
    doble (doble (doble (2)))
 -}

many' :: Int -> (a -> a) -> a -> a
many' 0 f = id
many' n f = f . many (n-1) f

{-
    many' 3 doble = doble . many 2 doble
    doble . doble . many 1 doble
    doble . doble . doble . many 0 doble
    doble . doble . doble . id 
    doble . doble . doble
-}

{- many' n f = f . (many (n-1) f) -}

-- Ejercicio 8

{- 
    e8a :: (Int -> Int) -> Int -> Int (NO)
    e8b :: (a -> b -> c) -> (a -> b) -> c (NO)
    e8c :: (a -> b, c -> d) -> (a, c) -> (b, d) (NO)
    e8d :: ((a, a) -> b) -> a -> b (NO)
    e8e :: (a -> b -> c) -> b -> a -> c (NO)
    e8f :: (a -> b) -> (a, a) -> (b, b) (NO)
    e8g :: (a -> b, a -> c) -> a -> (b, c) (NO)
    e8h :: (a -> b -> c) -> (a -> b) -> a -> c (CASI SEGURO)
    e8i :: a -> b -> a (NO)
-}

-- Ejercicio 9

cuadruple = doble . doble 
timesTwoPlusThree = (+3) . doble

fourTimes f  = f . f . f . f 

-- Ejercicio 10

(&&) True False