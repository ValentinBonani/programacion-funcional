{- seccion 2: 1 - 3 - 2. No hagas el 4 
seccion 3: no hacer
hacer todo lo de listas completo -}


-- Ejercicio 1

{-
    f :: [a] -> Int
    f [] = ...
    f (x:xs) = ... f xs 
-}

{- a. length :: [a] -> Int, que describe la cantidad de elementos de la lista. -}


length2 :: [a] -> Int
length2 [] = 0
length2 (_:xs) = 1 + length2 xs 

{- b. sum :: [Int] -> Int, que describe la suma de todos los elementos de la lista. -}

sum2 :: [Int] -> Int
sum2 [] = 0
sum2 (x:xs) = x + sum2 xs 

{- c. product :: [Int] -> Int, que describe el producto entre todos los elementos de la lista. -}

product2 :: [Int] -> Int
product2 [] = 0
product2 (x:xs) = x * product2 xs 

{- d. concat :: [[a]] -> [a], que describe la lista resultante de concatenar todas las listas que son elementos de la dada. -}

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (x:xs) = append x (concat2 xs)

{- e. elem :: Eq a => a -> [a] -> Bool, que indica si el elemento dado pertenece a la lista. -}

elem2 :: Eq a => a -> [a] -> Bool
elem2 _ [] = False 
elem2 a (x:xs) = x == a || elem2 a xs

{- f. all :: (a -> Bool) -> [a] -> Bool, que indica si todos los elementos de la lista cumplen el predicado dado. -}

all2 :: (a -> Bool) -> [a] -> Bool
all2 _ [] = True
all2 f (x:xs) = if(not (f x)) then False else all2 f xs

{- g. any :: (a -> Bool) -> [a] -> Bool, que indica si algún elemento de la lista cumple el predicado dado. -}

any2 :: (a -> Bool) -> [a] -> Bool
any2 _ [] = False
any2 f (x:xs) = f x || any2 f xs

{- h. count :: (a -> Bool) -> [a] -> Int, que describe la cantidad de elementos de la lista que cumplen el predicado dado. -}

cumple :: (a -> Bool) -> a -> Int
cumple f a = if(f a) then 1 else 0

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (x:xs) = cumple f x + count f xs

{- i. subset :: Eq a => [a] -> [a] -> Bool, que indica si todos los elementos de la primera lista se encuentran en la segunda. -}


subset :: Eq a => [a] -> [a] -> Bool
subset [] _ = True
subset (x:xs) ys = elem2 x ys && subset xs ys

{- j. (++) :: [a] -> [a] -> [a], que describe el resultado de agregar los elementos de la primera lista adelante de los elementos de la segunda. -}

(+++) :: [a] -> [a] -> [a]
(+++) [] ys = ys
(+++) (x:xs) ys = x : (+++) xs ys

{- k. reverse :: [a] -> [a], que describe la lista que tiene los elementos en el orden inverso a la lista dada. -}

reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (x:xs) =  reverse2 xs ++ [x]

{- l. zip :: [a] -> [b] -> [(a,b)], que describe la lista resultante de juntar de a pares los elementos de ambas listas, según la posición que comparten en cada una. -}

zip2 :: [a] -> [b] -> [(a,b)]
zip2 [] [] = []
zip2 (x:xs) (y:ys) = (x,y) : (zip2 xs ys)

{- m. unzip :: [(a,b)] -> ([a],[b]), que describe el par de listas que resulta de desarmar la lista dada; la primera componente del resultado se
corresponde con las primeras componentes de los pares dados, y la segunda componente con las segundas componentes de dichos pares. -}

aux2 :: ([a],[b]) -> ([a],[b]) -> ([a],[b])
aux2 (x1,y1) (x2,y2) = (append x1 x2, append y1 y2)

unzip2 :: [(a,b)] -> ([a],[b])
unzip2 []  = ([],[])
unzip2 ((x,y):xs) = aux2 ([x],[y]) (unzip2 xs)

-- Ejercicio 2

{- a. para todo xs. para todo ys.
length (xs ++ ys) = length xs + length ys -}

Prop: ¿length (xs ++ ys) = length xs + length ys, para todo xs :: [a], ys :: [b] ?
Case Base: 
xs = [], ys = []

length ([] ++ []) = length [] + length []
-- lado izq
length ([] ++ [])
-- def ++
length []
-- def length
0
-- lado der
length [] + length []
-- def lenght * 2
0 + 0
-- matematica quantica
0

Caso inductivo xs = (x:xss)
HI: ¡length (xss ++ ys) = length xss + length ys!
TI ¿length ((x:xss) ++ ys) = length (x:xss) + length ys?

-- NOTA siempre arrancar de la tesis

-- Lado der
length (x:xss) + length ys
-- def length
1 + length xss + length ys
-- HI
1 + length (xss ++ ys)

-- Lado izq
length ((x:xss) ++ ys)
-- def de ++
length (x : xss ++ ys)
--def de length
1 + length (xss ++ ys)

-- Llegamos a lo mismo

{- b. count (const True) = length -}

count (const True) = length
count (const True) l = length l
2 casos
Caso Base: l = []
count (const True) [] = length []
-- lado izq
count (const True) []
-- def count
0
-- lado der
length []
-- def length
0

Caso inductivo: l = (x:xs)
HI: ¡count (const True) xs = length xs!
TI: ¿count (const True) (x:xs) = length (x:xs)?
-- lado izq 
count (const True) (x:xs)
--def count
cumple (const True) x + count (const True) xs
-- def cumple
if((const True) x) then 1 else 0 + count (const True) xs
--def de if
1 + count (const True) xs
-- HI
1 + length xs

--lado der
length (x:xs)
--def de lenght
1 + length xs

-- Llegamos a lo mismo

{- c. elem = any . (==) -}

elem = any . (==)
elem e l = (any . (==)) e l
-- EJ PARA MI any ((==) 1) [1]
2 Casos:

Caso Base: l = []
elem e [] = (any . (==)) e []
-- lado izq
elem e []
-- def elem
False

-- Lado der
(any . (==)) e []
-- def (.)
any ((==) e) []
-- def any
False

Caso Inductivo: l = (x:xs)
HI: ¡elem e xs = (any . (==)) e xs!
TI: ¿elem e (x:xs) = (any . (==)) e (x:xs)?

--lado izq

elem e (x:xs)
--def de elem
x == e || elem2 e xs
-- HI
x == e || (any . (==)) e xs
-- def .
x == e || any ((==) e) xs



-- lado der
(any . (==)) e (x:xs)
-- def .
any ((==) e) (x:xs)
-- def de any
(==) e x || any ((==) e) xs
-- def de (==)
x == e || any ((==) e) xs

-- Llegamos a lo mismo

-- TO DO TODO

{- d. para todo x. any (elem x) = elem x . concat -}

{- e. para todo xs. para todo ys. subset xs ys = all (flip elem ys) xs -}

{- f. all null = null . concat -}

{- g. length = length . reverse -}

{- h. para todo xs. para todo ys. -}

{- reverse (xs ++ ys) = reverse ys ++ reverse xs -}

{- i. para todo xs. para todo ys. -}

{- all p (xs++ys) = all p (reverse xs) && all p (reverse ys) -}

{- j. para todo xs. para todo ys. unzip (zip xs ys) = (xs, ys) -}



-- Seccion 2
-- EJ 1 S2
data N = Z | S N


evalN :: N -> Int
evalN Z = 0
evalN (S n) = 1 + evalN n

addN :: N -> N -> N
addN Z n2 = n2
addN (S n) n2 = S addN n n2

prodN :: N -> N -> N
prodN Z n2 = Z
prodN (S n) n2 = addN n2 (prodN n n2)

int2N :: Int -> N
int2N 0 = Z
int2N n = S int2N (n-1)

--  demostrar
evalN (addN n1 n2) = evalN n1 + evalN n2
2 casos
caso Base, n1 = Z
evalN (addN Z n2) = evalN Z + evalN n2
-- Lado izq
evalN (addN Z n2)
-- Def addN
evalN n2

-- Lado Der
evalN Z + evalN n2
-- def evalN izq
0 + evalN n2
--matemat
evalN n2

caso Inductivo, n1 = (S n)
¡HI: evalN (addN n n2) = evalN n + evalN n2!
¿TI: evalN (addN (S n) n2) = evalN (S n) + evalN n2?
-- Lado izq
evalN (addN (S n) n2)
--def de addN
evalN (S (addN n n2))
--def evalN
1 + evalN (addN n n2)
-- HI 
1 + evalN n + evalN n2

-- Lado Der
evalN (S n) + evalN n2?
--def evalN izquierdo
1 + evalN n + evalN n2

-- Llegamos a lo mismo

data DigDec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving Show
type NDec = [DigDec]

normalizarND :: NDec -> NDec
normalizarND [] = []
normalizarND (x:xs) = x == D0 || normalizarND  xs