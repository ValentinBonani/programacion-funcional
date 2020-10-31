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
heightT EmptyT = 1
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

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = [[]]
todosLosCaminos (NodeT e ab1 ab2) = map (e:) ((todosLosCaminos ab1) ++++ (todosLosCaminos ab2))

