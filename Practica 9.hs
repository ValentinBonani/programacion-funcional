-- Ejercicio 1

data EA = Const Int 
    | BOp BinOp EA EA
    deriving Show

data BinOp = Sum | Mul deriving Show

data ExpA = Cte Int
    | Suma ExpA ExpA
    | Prod ExpA ExpA
    deriving Show

evalEA :: EA -> Int
evalEA (Const n) = n
evalEA (BOp Sum ea1 ea2) = evalEA ea1 + evalEA ea2
evalEA (BOp Mul ea1 ea2) = evalEA ea1 * evalEA ea2

ea2ExpA :: EA -> ExpA
ea2ExpA (Const n) = Cte n
ea2ExpA (BOp Sum ea1 ea2) = Suma (ea2ExpA ea1) (ea2ExpA ea2)
ea2ExpA (BOp Mul ea1 ea2) = Prod (ea2ExpA ea1) (ea2ExpA ea2)

expA2ea :: ExpA -> EA
expA2ea (Cte n) = Const n
expA2ea (Suma e1 e2) = BOp Sum (expA2ea e1) (expA2ea e2)
expA2ea (Prod e1 e2) = BOp Mul (expA2ea e1) (expA2ea e2)


expTest :: EA
expTest = BOp Mul (Const 5) (BOp Sum (Const 2) (Const 3))

--- TODO DEMOSTRACIONES

-- Ejercicio 2

data Arbol a b = Hoja b 
    | Nodo a (Arbol a b) (Arbol a b)
    deriving Show

cantidadDeHojas :: Arbol a b -> Int
cantidadDeHojas (Hoja _) = 1
cantidadDeHojas (Nodo _ ab1 ab2) = cantidadDeHojas ab1 + cantidadDeHojas ab2

arbol :: Arbol Integer Integer
arbol = Nodo 1 (Nodo 3 (Hoja 4) (Hoja 5)) (Nodo 3 (Hoja 4) (Hoja 5))

cantidadDeNodos :: Arbol a b -> Int
cantidadDeNodos (Hoja _) = 0
cantidadDeNodos (Nodo _ ab1 ab2) = 1 + cantidadDeNodos ab1 + cantidadDeNodos ab2

cantidadDeConstructores :: Arbol a b -> Int
cantidadDeConstructores (Hoja _) = 1
cantidadDeConstructores (Nodo _ ab1 ab2) = 1 + cantidadDeConstructores ab1 + cantidadDeConstructores ab2

ea2Arbol :: EA -> Arbol BinOp Int
ea2Arbol (Const n) = Hoja n
ea2Arbol (BOp Sum ea1 ea2) = Nodo Sum (ea2Arbol ea1) (ea2Arbol ea2)
ea2Arbol (BOp Mul ea1 ea2) = Nodo Mul (ea2Arbol ea1) (ea2Arbol ea2)

-- TODO Demostraciones

-- Ejercicio 3
data Tree a = EmptyT 
    | NodeT a (Tree a) (Tree a)
    deriving Show

arbolT :: Tree Int
arbolT = NodeT 10 (NodeT 10 EmptyT EmptyT) (NodeT 5 EmptyT (NodeT 2 EmptyT EmptyT))


arbolT2 = NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT)  (NodeT 4 EmptyT EmptyT)) (NodeT 5 (NodeT 7 (NodeT 6 EmptyT EmptyT) (NodeT 9 EmptyT EmptyT)) (NodeT 8 EmptyT EmptyT))

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT n ab1 ab2) = n + sumarT ab1 + sumarT ab2

sizeT :: Tree a -> Int
sizeT EmptyT = 1
sizeT (NodeT _ ab1 ab2) = 1 + sizeT ab1 + sizeT ab2

anyT :: (a -> Bool) -> Tree a -> Bool
anyT _ EmptyT = False
anyT f (NodeT n ab1 ab2) = f n || anyT f ab1 || anyT f ab2

countT :: (a -> Bool) -> Tree a -> Int
countT _ EmptyT = 0
countT f (NodeT n ab1 ab2) =
    let act = if(f n) then 1 else 0
    in act + countT f ab1 + countT f ab2

countLeaves :: Tree a -> Int
countLeaves EmptyT = 1
countLeaves (NodeT _ ab1 ab2) = countLeaves ab1 + countLeaves ab2

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT _ ab1 ab2) = max (1 + heightT ab1) (1 + heightT ab2)

inOrder :: Tree a -> [a]
inOrder EmptyT = []
inOrder (NodeT n ab1 ab2) = inOrder ab1 ++ [n] ++ inOrder ab2

auxMerge :: [[a]] -> [[a]] -> [[a]]
auxMerge [] [] = [] 
auxMerge [] ys = ys
auxMerge xs [] = xs
auxMerge (x:xs) (y:ys) = (x ++ y) : auxMerge xs ys 

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT n ab1 ab2) = [n] : (auxMerge (listPerLevel ab1) (listPerLevel ab2))


mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT n ab1 ab2) = NodeT n (mirrorT ab2) (mirrorT ab1)

elemIndex :: Int -> [a] -> Maybe a
elemIndex _ [] = Nothing
elemIndex 0 (x:_) = Just x
elemIndex n a = elemIndex (n-1) a

levelN :: Int -> Tree a -> [a]
levelN 0 (NodeT n _ _) = [n]
levelN _ EmptyT = []
levelN act (NodeT _ ab1 ab2) = (levelN (act-1) ab1) ++ (levelN (act-1) ab2) 

(++++) :: [[a]] -> [[a]] -> [[a]]
(++++) [[]] [[]] = [[]]
(++++) xs ys = xs ++ ys

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT e ab1 ab2) = if(heightT ab1 > heightT ab2) then e : ramaMasLarga ab1 else e : ramaMasLarga ab2


todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = [[]]
todosLosCaminos (NodeT e ab1 ab2) = map (e:) ((todosLosCaminos ab1) ++++ (todosLosCaminos ab2))
    
-- EJ4Bi

PROP: heightT = length . ramaMasLarga
DEM: por ppio de ext.
    ¿para todo ab. heightT ab = (length . ramaMasLarga) ab?
Por definición de composición, es equivalente a
    ¿para todo ab. heightT ab = length (ramaMasLarga ab)? 


Caso Base)
    heightT EmptyT = length (ramaMasLarga EmptyT)
    -- lado izq
    heightT EmptyT
    -- def height
    0

    -- lado der
    length (ramaMasLarga EmptyT)
    -- def ramaMasLarga
    length ([])
    -- def length 
    0

Caso Ind)
        ¡HI.1: heightT ab1 = length (ramaMasLarga ab1)! 😎
        ¡HI.2: heightT ab2 = length (ramaMasLarga ab2)! 😎 😎
        ¿TI: heightT (NodeT e ab1 ab2)  = length (ramaMasLarga (NodeT e ab1 ab2))?   😎 😎 😎
        -- lado derecho
            length (ramaMasLarga (NodeT e ab1 ab2))
        -- def ramaMasLarga
            length (if(heightT ab1 > heightT ab2) then e : ramaMasLarga ab1 else e : ramaMasLarga ab2)
        -- lema if-lenght
            if(heightT ab1 > heightT ab2) then length (e : ramaMasLarga ab1) else length (e : ramaMasLarga ab2))
        -- def Lenght x2
            if(heightT ab1 > heightT ab2) then (1 + length (ramaMasLarga ab1)) else (1 + length ( ramaMasLarga ab2))
        -- HI.1 && HI.2
            if(heightT ab1 > heightT ab2) then (1 + heightT ab1) else (1 + heightT ab2)


        -- lado izq
            heightT (NodeT e ab1 ab2)
        -- def heightT
            max (1 + heightT ab1) (1 + heightT ab2)
        -- def max
            if((1 + heightT ab1) > (1 + heightT ab2)) then (1 + heightT ab1) else (1 + heightT ab2)
        -- matematica del >
            if(heightT ab1 > heightT ab2) then (1 + heightT ab1) else (1 + heightT ab2)
            

        -- Lema                
        para todo a::Bool y para todo b,c :: [d]
            length (if(a) then b else c) = if(a) then length b else length c
        2 Casos) 
        a = False
            length (if(False) then b else c) = if(False) then length b else length c
            -- def if
            length c = length c
        
        a = True
            length (if(False) then b else c) = if(False) then length b else length c
            -- def if
            length b = length b

data Tree a = EmptyT 
    | NodeT a (Tree a) (Tree a)
    deriving Show

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT _ ab1 ab2) = max (1 + heightT ab1) (1 + heightT ab2)

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT e ab1 ab2) = if(heightT ab1 > heightT ab2) then e : ramaMasLarga ab1 else e : ramaMasLarga ab2

-- EJ4Bii

PROP: reverse . inOrder = inOrder . mirrorT
DEM: por ppio de ext.
    ¿para todo ab. (reverse . inOrder) ab = (inOrder . mirrorT) ab?
Por definición de composición, es equivalente a
    ¿para todo ab. reverse (inOrder ab) = inOrder (mirrorT ab)? 

Caso Base)
    reverse (inOrder EmptyT) = inOrder (mirrorT EmptyT)
    -- Lado Izq 
        reverse (inOrder EmptyT)
    -- def inorder
        reverse ([])
    -- def reverse
        []

    -- Lado Derecho
        inOrder (mirrorT EmptyT)
    -- Def mirrorT
        inOrder EmptyT
    -- def inOrder
        []

Caso Ind)
        ¡HI.1: reverse (inOrder ab1) = inOrder (mirrorT ab1)! 😎
        ¡HI.2: reverse (inOrder ab2) = inOrder (mirrorT ab2)! 😎 😎
        ¿TI: reverse (inOrder (NodeT n ab1 ab2)) = inOrder (mirrorT (NodeT n ab1 ab2))?   😎 😎 😎

        -- izq
            reverse (inOrder (NodeT n ab1 ab2))
        -- def inorder
            reverse (inOrder ab1 ++ [n] ++ inOrder ab2)
        -- Lema reversero
            reverse (inOrder ab2) ++ reverse [n] ++ reverse (inOrder ab1)
        -- def reverse
            reverse (inOrder ab2) ++ (reverse [] ++ [n]) ++ reverse (inOrder ab1)
        -- def reverse
            reverse (inOrder ab2) ++ ([] ++ [n]) ++ reverse (inOrder ab1)
        -- def ++
            reverse (inOrder ab2) ++ [n] ++ reverse (inOrder ab1)
        
        -- lado der
            inOrder (mirrorT (NodeT n ab1 ab2))
        -- def mirrorT
            inOrder (NodeT n (mirrorT ab2) (mirrorT ab1))
        -- def inOrder
            inOrder (mirrorT ab2) ++ [n] ++ inOrder (mirrorT ab1)
        -- HI.1 && HI.2
            reverse (inOrder ab2) ++ [n] ++ reverse (inOrder ab1)

        lema reversero
        reverse (xs ++ ys ++ zs) = reverse zs ++ reverse ys ++ reverse xs

        Caso Base)
        reverse ([] ++ [] ++ []) = reverse [] ++ reverse [] ++ reverse []
        -- lado izq 
            reverse ([] ++ [] ++ [])
        -- def ++ x2
            reverse ([])
        -- def reverse
            []
        
        
        -- lado der
            reverse [] ++ reverse [] ++ reverse []
        -- dedf reverse * 3
            [] ++ [] ++ []
        -- def ++ *2
            []

        Caso Ind)
        ¡HI: reverse (xs ++ ys ++ zs) = reverse zs ++ reverse ys ++ reverse xs! 😎
        ¿TI: reverse ((x:xs) ++ (y:ys) ++ (z:zs)) = reverse (z:zs) ++ reverse (y:ys) ++ reverse (x:xs)?   😎 😎 😎
        -- izq 
            reverse ((x:xs) ++ (y:ys) ++ (z:zs))
        -- def ++
            reverse (x: (xs ++ (y:ys)) ++ (z:zs))
        -- def reverse
            reverse ((xs ++ (y:ys)) ++ (z:zs)) ++ [x]


        -- lado derecho
            reverse (z:zs) ++ reverse (y:ys) ++ reverse (x:xs)
        -- def reverse *3
            reverse zs ++ [z] ++ reverse ys ++ [y] ++ reverse xs ++ [x]

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT n ab1 ab2) = NodeT n (mirrorT ab2) (mirrorT ab1)

inOrder :: Tree a -> [a]
inOrder EmptyT = []
inOrder (NodeT n ab1 ab2) = inOrder ab1 ++ [n] ++ inOrder ab2

reverse :: [a] -> [a]
reverse EmptyT = []
reverse (x:xs) = reverse xs ++ [x]
