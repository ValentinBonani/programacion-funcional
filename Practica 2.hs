-- Ejercicio 1 


first :: (a,b) -> a
first (x,y) = x

second :: (a,b) -> b
second (x,y) = y

apply :: (a -> b) -> (a -> b)
apply f = g
    where g x = f x

{- twice :: (a -> a) -> (a -> a) -}
twice f = g
    where g x = f (f x)

doble :: Int -> Int
doble x = x + x

swap :: (a,b) -> (b,a)
swap (x, y) = (y, x)

uflip :: ((a,b) -> c) -> ((b,a) -> c)
uflip f = g
    where g p = f (swap p)

-- Ejercicio 2

af :: (a,b) -> a
af = apply first

fsu :: (a,b) -> (b,a)
fsu = first (swap, uflip)

td :: Int -> Int
td = twice doble

tt :: (a -> a) -> (a -> a)
tt = twice twice

tu :: ((a,a) -> c) -> ((a,a) -> c)
tu = twice uflip

ts :: (a,a) -> (a,a)
ts = twice swap

us :: (b,a) -> (b,a)
us = uflip swap

tts :: (a,a) -> (a,a)
tts = (twice twice) swap 

-- Ejercicio 3

const :: a -> (b -> a)
const x = g
    where g y = x

appDup :: ((a,a) -> c) -> (a -> c)
appDup f = g
    where g x = f (x, x)

appFork :: (a -> b , a -> c) -> (a -> (b,c))
appFork (f, g) = h
    where h x = (f x, g x)

appPar :: (a -> b, c -> d) -> ((a,c) -> (b,d))
appPar (f, g) = h
    where h (x, y) = (f x, g y)

appDist :: (a -> b) -> ((a,a) -> (b,b))
appDist f = g
    where g (x, y) = (f x, f y)

flip :: (a -> (b -> c)) -> (b -> (a -> c))
flip f = h 
    where { h x = k
        where k y = (f y) x }
                     
subst :: (a -> (b -> c)) -> ((a -> b) -> (a -> c))
subst f = h
    where { h g = k
          where k x = (f x) (g x) }

-- Ejercicio 4

{- a. 1 && 2 == 2  No tipa-}

{- b. 1 + if 3 < 5 then 3 else 5 :: Int -}

{- c. let par = (True, 4) in (if first par then first par else second par) No Tipa  -}

{- d. (doble doble) 5 :: No tipa -}

{- e. doble (doble 5) :: Int -}

{- f. twice first :: No tipa porque no puede igualar (a,b) = a -}

{- g. (twice doble) doble :: NO TIPA -}

{- h. (twice twice) first :: No tipa -}

{- i. apply apply :: (a -> b) -> (a -> b) -}

-- Ejercicio 5

a :: Bool
a = False
a' = True

b :: (Int, Int)
b = (1,2)
b' = (1,3)

c :: Char -> Int
c x = 2
c' = \x -> 2

d :: (Int, Char) -> Bool
d = \(x,y) -> False
d' (x,y) = True

e :: (Int -> Int) -> Int
e = \f -> 0

e' = \f -> 2

f :: (Bool -> Bool, Int)
f = ((\x -> False), 2)
f' = g
    where g = ((\x -> True), 3)

g :: a -> Bool
g x = False
g' = \x -> True == False

