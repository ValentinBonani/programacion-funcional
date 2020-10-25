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
concat2 (x:xs) = x ++ (concat2 xs)

{- e. elem :: Eq a => a -> [a] -> Bool, que indica si el elemento dado pertenece a la lista. -}

elem2 :: Eq a => a -> [a] -> Bool
elem2 _ [] = False 
elem2 a (x:xs) = x == a || elem2 a xs

{- f. all :: (a -> Bool) -> [a] -> Bool, que indica si todos los elementos de la lista cumplen el predicado dado. -}

all2 :: (a -> Bool) -> [a] -> Bool
all2 _ [] = True
all2 f (x:xs) = f x && all2 f xs

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
zip2 (x:xs) [] = []
zip2 [] (y:ys) = []
zip2 (x:xs) (y:ys) = (x,y) : (zip2 xs ys)

{- m. unzip :: [(a,b)] -> ([a],[b]), que describe el par de listas que resulta de desarmar la lista dada; la primera componente del resultado se
corresponde con las primeras componentes de los pares dados, y la segunda componente con las segundas componentes de dichos pares. -}

aux2 :: ([a],[b]) -> ([a],[b]) -> ([a],[b])
aux2 (x1,y1) (x2,y2) = (append x1 x2, append y1 y2)

unzip2 :: [(a,b)] -> ([a],[b])
unzip2 []  = ([],[])
unzip2 (x:xs) [] = ([],[])
unzip2 [] (y:ys) = ([],[])
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

PROP: any (elem x) = (elem x) . concat
DEM: por ppio de ext.
    ¿para todo l. any (elem x) l = ((elem x) . concat) l?
Por definición de composición, es equivalente a
    ¿para todo l. any (elem x) l = elem x (concat l)?
Sea l :: [[a]]. Por induccion en la estructura de xs

Caso base) ¿any (elem x) [] = elem x (concat [])?
    -- Lado izquierdo
        any (elem x) []
    -- Def de any
        False
        
    -- Lado derecho
        elem x (concat [])
    -- Def de concat
        elem x []
    -- Def de elem
        False
            
Caso Inductivo) 
    HI) ¡any (elem x) xs = elem x (concat xs)!
    TI) ¿any (elem x) (x':xs) = elem x (concat (x':xs))?
    -- Lado Izq
        any (elem x) (x':xs)
    -- Def any
        (elem x) x' || any (elem x) xs
    -- Por HI
        elem x x' || elem x (concat xs)

        
    -- Lado Der
        elem x (concat (x':xs))
    -- Def de concat
        elem x (x' ++ (concat xs))
    -- Por lema
        elem x x' || elem x (concat xs)

-- Vale el caso

-- Lema
para todo xs. para todo ys. 
    elem e (xs ++ ys) == elem e xs || elem e ys
Dem: Sean xs e ys listas cualesquiera
Caso Base) elem e ([] ++ ys) == elem e [] || elem e ys
            -- Lado Izq
            elem e ([] ++ ys)
            -- Def ++
            elem e ys
            
            -- Lado Der
            elem e [] || elem e ys
            -- Def elem izq
            False || elem e ys
            -- Def de ||
            elem e ys

Caso Ind)
        HI) ¡elem e (xss ++ ys) == elem e xss || elem e ys!
        TI) ¿elem e ((x:xss) ++ ys) == elem e (x:xss) || elem e ys?

    -- Lado Izq
        elem e ((x:xss) ++ ys)
    -- def ++
        elem e (x : (xss ++ ys))
    -- def elem
        x == e || elem e (xss ++ ys)
    -- HI
        x == e || elem e xss || elem e ys

    -- Lado der
        elem e (x:xss) || elem e ys
    -- def de elem de la izq
        x == e || elem e xss || elem e ys

{- e. para todo xs. para todo ys. subset xs ys = all (flip elem ys) xs -}

PROP: subset xs ys = all (flip elem ys) xs
Sea xs,ys una lista cualesquiera

Caso Base)
        subset [] ys = all (flip elem ys) []
    -- Lado izq
        subset [] ys
    -- Def de subster
        True

    -- Lado der
        all (flip elem ys) []
    -- Def de all
        True

Caso Ind)
    HI) ¡subset xss ys = all (flip elem ys) xss!
    TI) ¿subset (x:xss) ys = all (flip elem ys) (x:xss)?

    -- Lado izq
        subset (x:xss) ys
    -- def de subset
        elem x ys && subset xss ys
    -- HI
        elem x ys && all (flip elem ys) xss

    -- Lado der
        all (flip elem ys) (x:xss)
    -- Def de all
        (flip elem ys) x && all (flip elem ys) xss
    -- Def de flip
        elem x ys && all (flip elem ys) xss
    
        
{- f. all null = null . concat -}

PROP: all null = null . concat
DEM: por ppio de ext.
    ¿para todo xs. all null xs = (null . concat) xs?
Por definición de composición, es equivalente a
    ¿para todo xs. all null xs = null (concat xs)?

Caso Base)
        all null [] = null (concat [])

    -- Lado izq
        all null []
    -- Def all
        True

    -- Lado der
        null (concat [])
    -- def concat
        null []
    -- Def null
        True

Caso Ind)
    HI) ¡all null xss = null (concat xss)!
    TI) ¿all null (x:xss) = null (concat (x:xss))?
    
    -- Lado izq
        all null (x:xss)
    -- Def all
        null x && all null xss
    -- HI MAN
        null x && null (concat xss)
        
    -- Lado Der
        null (concat (x:xss))
    -- Def Concat
        null (x ++ (concat xss))
    -- LEMA QUE NO HEMOS DEMOSTRADO PERO LA VERDAD SUPERA A LA FICCION
        null x && null (concat xss)

-- LEMA VALENTONGUERO
null (xs ++ ys) = null xs && null ys -- y seguro que esta bien

para todo xs. para todo ys. 
    elem e (xs ++ ys) == elem e xs || elem e ys
Dem: Sean xs e ys listas cualesquiera
Caso Base) elem e ([] ++ ys) == elem e [] || elem e ys



null :: [a] -> Bool
null [] = True
null (x:xs) = False


{- g. length = length . reverse -}

PROP: length = length . reverse
DEM: por ppio de ext.
    ¿para todo xs. length xs = (length . reverse) xs?
Por definición de composición, es equivalente a
    ¿para todo xs. length xs = length (reverse xs)?

Caso Base)
        length [] = length (reverse [])
    -- lado Der
        length (reverse [])
    -- Def reverse
        length []

Caso Ind)
    HI) ¡length xss = length (reverse xss)!
    TI) ¿length (x:xss) = length (reverse (x:xss))?

    -- Lado Der
        length (reverse (x:xss))
    --  Def de reverse
        length (reverse xss ++ [x])
    -- Por Lema RUBIUMAXIMUS666
        length (reverse xss) + length [x]
    -- Def de length de la derecha
        length (reverse xss) + 1
    -- HI
        length xss + 1
    -- Por fisicia cuantimistica
        1 + length xss

    -- Lado Izq
        length (x:xss)
    -- Def de length
        1 + length xss
    

-- LEMA RUBIUMAXIMUS666
para todo xs,ys.    length (xs ++ ys) = length xs + length ys
Demostracion
Caso Base)  length ([] ++ ys) = length [] + length ys
            -- Lado Izq
            length ([] ++ ys)
            -- Def ++
            length ys
            -- def length
            length [] + length ys

Caso Ind)
            HI) ¡length (xss ++ ys) = length xss + length ys!
            TI) ¿length ((x:xss) ++ ys) = length (x:xss) + length ys?
            -- Lado Izq
                length ((x:xss) ++ ys)
            -- Def ++
                length(x : xss ++ ys)
            -- Def lenght
                1 + length (xss ++ ys)
            -- HI 
                1 + length xss + length ys

            -- Lado der 
                length (x:xss) + length ys
            -- def de length
                1 + length xss + length ys
                


        length [] = 0
        length (x:xs) = 1 + length xss 
            


{- h. para todo xs. para todo ys. reverse (xs ++ ys) = reverse ys ++ reverse xs -}
    
    PROP: reverse (xs ++ ys) = reverse ys ++ reverse xs

Caso Base)  
    
        reverse ([] ++ ys) = reverse ys ++ reverse []
    -- Lado Izq
        reverse ([] ++ ys)
    -- def ++
        reverse ys    

    -- lado capitalista
        reverse ys ++ reverse []
    -- def reverse der
        reverse ys ++ []
    -- Por lema de abajo
        reverse ys

-- Lema super al pedo pero necesario
para toda xs. xs = xs ++ []
DEM)
Caso Basic) [] = [] ++ []
    -- Lado derecho
    [] ++ []
    -- Def ++
    []
Caso ind)
    HI) ¡ xss = xss ++ [] !
    TI) ¿ x:xss = (x:xss) ++ [] ?
    -- lado der
        (x:xss) ++ []
    -- def de ++
        x : xss ++ []
    -- por el HI
        x : xss


{- i. para todo xs. para todo ys.
        all p (xs++ys) = all p (reverse xs) && all p (reverse ys) -}

    all p (xs++ys) = all p (reverse xs) && all p (reverse ys)
    por lema de FIDEL YA ESTA

{- j. para todo xs. para todo ys. unzip (zip xs ys) = (xs, ys) -}

PROP: unzip (zip xs ys) = (xs, ys)

3 casos bases)
    1)
        unzip (zip [] []) = ([], [])
    -- lado izq
        unzip (zip [] [])
    -- def de zip
        unzip []
    -- def de unzip
        ([], [])

    2)
        unzip (zip xs []) = ([], [])
    -- Lado izq
        unzip (zip xs [])
    -- Def zip
        unzip []
    -- Def unzip
        ([], [])
        
    3)
        unzip (zip [] ys) = ([], [])
    -- Lado izq
        unzip (zip [] ys)
    -- Def zip
        unzip []
    -- Def unzip
        ([], [])

Caso Ind)
    HI) ¡unzip (zip xss yss) = (xss, yss)!
    TI) ¿unzip (zip (x:xss) (y:yss)) = ((x:xss), (y:yss))?

    -- Lado Del Caño
        unzip (zip (x:xss) (y:yss))
    -- def de zip
        unzip ((x,y) : (zip xss yss))
    -- def de unzip
        aux2 ([x],[y]) (unzip (zip xss yss))
    -- HI 
        aux2 ([x],[y]) (xss, yss)
    -- def aux2
        ([x] ++ xss, [y] ++ yss)
    -- def ++ x2
        (x : [] ++ xss, y: [] ++ yss)
    -- def ++ x2
        ((x:xss), (y:yss))

    
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

{- ii. para todo n1. para todo n2. evalN (prodN n1 n2) = evalN n1 * evalN n2 -}

PROP: evalN (prodN n1 n2) = evalN n1 * evalN n2
Caso Base)
    evalN (prodN Z n2) = evalN Z * evalN n2
    -- Lado izq
        evalN (prodN Z n2)
    -- Def prodN
        evalN Z
    -- Def evalN
        0

    --lado der 
        evalN Z * evalN n2
    -- def evalN
        0 * evalN n2
    -- matemistica maximisticus
        0
Caso Inducitvo)
    ¡HI: evalN (prodN n n2) = evalN n * evalN n2!
    ¿TI: evalN (prodN (S n) n2) = evalN (S n) * evalN n2?
    -- lado marx
        evalN (prodN (S n) n2)
    -- def de prodN
        evaln (addN n2 (prodN n n2))
    -- Por lema marx drugs extreme
        evaln n2 + evaln (prodN n n2)


    -- Lema marx drugs extreme
        para todo n1 :: N, n2 :: N -> evaln (addN (n1 n2)) = evalN n1 + evalN n2

    -- Lado der
        evalN (S n) * evalN n2
    -- def de evalN
        (1 + evalN n) * evalN n2
    -- mat
        1 * evalN n2 + evalN n * evalN n2
    -- HI y mat
        evalN n2 + evalN (prodN n n2)

    -- Lema marx drugs extreme
    para todo n1 :: N, n2 :: N -> evaln (addN n1 n2) = evalN n1 + evalN n2
    
    Demostracion)
    
    Caso Base) evaln (addN (Z n2)) = evalN Z + evalN n2
    -- Lado marx zuckmemberg
        evaln addN (Z n2)
    -- Def addN
        evaln n2

    -- Lado donald trump
        evalN Z + evalN n2
    -- def evalN izq
        0 + evalN n2
    -- Matematica de hardvard
        evalN n2

Caso Ind)
    ¡HI: evalN (addN n n2) = evalN n + evalN n2!
    ¿TI: evalN (addN (S n) n2) = evalN (S n) + evalN n2?
    -- Lado kk
        evalN (addN (S n) n2)
    -- def de addn
        evalN (S addN n n2)
    -- def evalN
        1 + evalN (addN n n2)
    -- HIPO
        1 + evalN n + evalN n2
    -- Def evalN
        evalN (S n) + evalN n2

{- iii. int2N . evalN = id -}

PROP: int2N . evalN = id
DEM: por ppio de ext.
    ¿para todo n :: N. (int2N . evalN) n = id n?
Por definición de composición, es equivalente a
    ¿para todo xs. int2N (evalN n) = id n?


Caso Basing Ze)
    int2N (evalN Z) = id Z
    -- lado chinatown
        int2N (evalN Z)
    -- def evalN
        int2M 0
    -- def de int2M
        Z
    -- def de id
        id Z

caso ind)
    ¡HI: int2N (evalN n) = id n!
    ¿TI: int2N (evalN (S n)) = id (S n)? 😎😎😎😎😎

    --lado mother russia
        int2N (evalN (S n))
    -- def de evalN
        int2N (1 + evalN n)
    -- lema lenin
        addn (int2N 1) (int2N evalN n)
    -- HIPOPITAs
        addn (int2N 1) (id n)
    -- def de int2N
        addn (S Z) (id n)
    -- def de id
        addn (S Z) n
    -- def de addn
        S addN Z n
    -- def addN
        S n
    -- def id
        id (S n)

    -- lema lenin
    int2N (n1 + n2) = addn (int2N n1) (int2N n2)

    addn 
    
    caso logBase)
    int2N (0 + n2) = addn (int2N 0) (int2N n2)
    -- lado izq
        int2N (0 + n2)
    -- mat
        int2N n2

    -- lado der
        addn (int2N 0) (int2N n2)
    -- def de int2N izq
        addn Z (int2N n2)
    -- def addn
        int2N n2

    caso inductivo)

        ¡HI: int2N (n + n2) = addn (int2N n) (int2N n2)!
        ¿TI: int2N ((n+1) + n2) = addn (int2N (n+1)) (int2N n2)?

    --lado left
        int2N ((n+1) + n2)
    -- def int2N
    S (int2N (n + n2))
    -- HI
    S (addn (int2N n) (int2N n2))

    --lado der
    addn (int2N (n+1)) (int2N n2)
    --def int2N izq
    addn (S int2N (n)) (int2N n2)
    -- def addn
    S (addn (int2N n) (int2N n2))


{- iv. evalN . int2N = id -}

PROP: evalN . int2N = id
DEM: por ppio de ext.
    ¿para todo n :: Int. (evalN . int2N) n = id n?
Por definición de composición, es equivalente a
    ¿para todo xs. evalN (int2N n) = id n?

Caso Basing Ze)
    evalN (int2N 0) = id 0
    
    -- lado chinatown
        evalN (int2N 0)
    -- def evalN
        evalN Z
    -- def de int2M
        0
    -- def de id
        id 0

caso ind)
    ¡HI: evalN (int2N n) = id n!
    ¿TI: evalN (int2N (n + 1)) = id (n + 1)? 😎😎😎😎😎

    --lado mother russia
        evalN (int2N (n + 1))
    -- def de int2N
        evalN (S (int2N n))
    -- def EvalN
        1 + evalN (int2N n)
    -- HI
        1 + id n
    -- def id
        1 + n
    -- mat
        n + 1
    --def id
        id (n + 1)



data DigDec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving Show
type NDec = [DigDec]

normalizarND :: NDec -> NDec
normalizarND [] = []
normalizarND (x:xs) = x == D0 || normalizarND  xs