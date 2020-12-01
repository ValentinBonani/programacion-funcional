data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA deriving Show

foldExpA :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA f _ _ (Cte x) = f x
foldExpA f g h (Suma e1 e2) = g (foldExpA f g h e1) (foldExpA f g h e2)
foldExpA f g h (Prod e1 e2) = h (foldExpA f g h e1) (foldExpA f g h e2)

expa1 = Suma (Cte (0)) (Prod (Cte 2) (Suma (Cte 0) (Cte 2)))

igualACero :: Int -> Int
igualACero 0 = 1
igualACero _ = 0

cantidadDeCeros :: ExpA -> Int
cantidadDeCeros = foldExpA (fromEnum . (==0)) (+) (+)

noTieneNegativosExplicitosExpA :: ExpA -> Bool
noTieneNegativosExplicitosExpA = foldExpA (>=0) (&&) (&&)

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Cte 0) e = e
simplificarSuma e (Cte 0) = e
simplificarSuma e1 e2 = Suma e1 e2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Cte 0) _ = Cte 0
simplificarProd (Cte 1) e = e
simplificarProd _ (Cte 0) = Cte 0
simplificarProd e (Cte 1) = e
simplificarProd e1 e2 = Prod e1 e2

simplificarExpA' :: ExpA -> ExpA
simplificarExpA' = foldExpA Cte simplificarSuma simplificarProd

evalExpA' :: ExpA -> Int
evalExpA' = foldExpA id (+) (*)

sumaToString :: String -> String -> String
sumaToString s1 s2 = s1 ++ "+" ++ s2

prodToString :: String -> String -> String
prodToString s1 s2 = "(" ++ s1 ++ "*" ++ s2 ++ ")"

showExpA :: ExpA -> String
showExpA = foldExpA show sumaToString prodToString

recExpA :: (Int -> b) -> (ExpA -> ExpA -> b -> b -> b) -> (ExpA -> ExpA -> b -> b -> b) -> ExpA -> b
recExpA f _ _ (Cte x) = f x
recExpA f g h (Suma e1 e2) = g e1 e2 (recExpA f g h e1) (recExpA f g h e2)
recExpA f g h (Prod e1 e2) = h e1 e2 (recExpA f g h e1) (recExpA f g h e2)

sumaCeros :: ExpA -> ExpA -> Int -> Int -> Int
sumaCeros (Cte 0) _ r1 r2 = 1 + r2
sumaCeros _ (Cte 0) r1 r2 = 1 + r1
sumaCeros _ _ r1 r2 = r1 + r2

cantDeSumaCeros :: ExpA -> Int
cantDeSumaCeros = recExpA (const 0) sumaCeros (\e1 e2 r1 r2-> r1 + r2)

-- Ejercio 2

data EA = Const Int | BOp BinOp EA EA deriving Show
data BinOp = Sum | Mul deriving Show

foldEA :: (Int -> b) -> (BinOp -> b -> b -> b) -> EA -> b
foldEA f g (Const n) = f n 
foldEA f g (BOp b e1 e2) = g b (foldEA f g e1) (foldEA f g e2)

-- Ejercicio 3

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

foldT :: b -> (a -> b -> b -> b) -> Tree a -> b
foldT n g EmptyT = n
foldT n g (NodeT e t1 t2) = g e (foldT n g t1) (foldT n g t2)

arbolitoNav :: Tree Int
arbolitoNav = NodeT 1 (NodeT 4 EmptyT EmptyT) (NodeT 2 (NodeT 3 EmptyT EmptyT) EmptyT) 

mapT :: (a -> b) -> Tree a -> Tree b
mapT f = foldT EmptyT (\e r1 r2 -> NodeT (f e) r1 r2)

sumT :: Tree Int -> Int
sumT = foldT 0 (\e r1 r2 -> e + r1 + r2)

sizeT :: Tree a -> Int
sizeT = foldT 1 (\e r1 r2 -> 1 + r1 + r2)

heightT :: Tree a -> Int
heightT = foldT 1 (\e r1 r2 -> 1 + max r1 r2)

preOrder :: Tree a -> [a]
preOrder = foldT [] (\e r1 r2 -> [e] ++ r1 ++ r2)

inOrder :: Tree a -> [a]
inOrder = foldT [] (\e r1 r2 -> r1 ++ [e] ++ r2)

postOrder :: Tree a -> [a]
postOrder = foldT [] (\e r1 r2 -> r1 ++ r2 ++ [e])

mirrorT :: Tree a -> Tree a
mirrorT = foldT EmptyT (\e r1 r2 -> NodeT e r2 r1)

countByT :: (a -> Bool) -> Tree a -> Int
countByT f = foldT 0 (\e r1 r2 -> fromEnum (f e) + r1 + r2)

joinTups :: ([a], [a]) -> ([a], [a]) -> ([a], [a])
joinTups (a, b) (c, d) = (a ++ c, b ++ d)

putInTup :: a -> ([a], [a]) -> Bool -> ([a], [a])
putInTup e (a, b) True = (e : a, b)
putInTup e (a, b) False = (a, e : b)

chiquito1 :: Tree Int
chiquito1 = NodeT 1 (NodeT 2 EmptyT EmptyT) (NodeT 3 EmptyT EmptyT)

chiquito2 :: Tree Int
chiquito2 = NodeT 2 (NodeT 3 EmptyT EmptyT) (NodeT 4 EmptyT EmptyT)

partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
partitionT f = foldT ([],[]) (\e r1 r2 -> putInTup e (joinTups r1 r2) (f e))


{- zipWithT :: (a->b->c) -> Tree a -> Tree b -> Tree c -}
{- zipWithT f = foldT (\t -> EmptyT) (\e1 r1 r2 -> f e  ) -}

{- applyT :: Tree (a -> b) -> Tree a -> Tree b
applyT EmptyT EmptyT = EmptyT
applyT (NodeT f h1 h2) (NodeT n e1 e2) = NodeT (f n) (applyT h1 e1) (applyT h2 e2)

zipWithT f a1 a2 = applyT (mapT f a1) a2 -}
{- foldT :: b -> (a -> b -> b -> b) -> Tree a -> b -}

joinTrees :: Tree a -> Tree b -> Tree (a, b)
joinTrees EmptyT _ = EmptyT
joinTrees (NodeT a1 e1i e2i) (NodeT a2 e1d e2d) = NodeT (a1, a2) (joinTrees e1i e1d) (joinTrees e2i e2d)

zipWithT :: (a->b->c) -> Tree a -> Tree b -> Tree c
zipWithT f t1 t2 = foldT EmptyT (NodeT . uncurry f) (joinTrees t1 t2)

recT :: b -> (a -> Tree a -> Tree a -> b -> b -> b) -> Tree a -> b
recT n g EmptyT = n
recT n g (NodeT e t1 t2) = g e t1 t2 (recT n g t1) (recT n g t2)

contains :: Eq a => a -> Tree a -> Bool
{- contains x = foldT False (\e r1 r2 -> (x==e) || r1 || r2 ) -}

contains e = (>0) . countByT (==e)

caminoHasta :: Eq a => a -> Tree a -> [a]
caminoHasta x = recT [] (\e t1 t2 r1 r2 -> if contains x t1 then e : r1 else if x==e then [e] else if contains x t2 then e : r2 else []) 
