-- Ejercicio 1


data NExp = Var Variable | NCte Int | NBOp NBinOp NExp NExp deriving Show
data NBinOp = Add | Sub | Mul | Div | Mod | Pow deriving Show
type Variable = String
data Memoria = Mem [(Variable,Int)] deriving Show

enBlanco :: Memoria 
enBlanco = Mem []

cuantoVale :: Variable -> Memoria -> Maybe Int
cuantoVale _ (Mem []) = Nothing
cuantoVale a (Mem ((key,value):xs)) = if(a == key) then Just value else cuantoVale a (Mem xs)

recordar :: Variable -> Int -> Memoria -> Memoria
recordar a x (Mem xs) = Mem ((a,x): xs) 

variables :: Memoria -> [Variable]
variables (Mem memoria) = map (\(x,_) -> x) memoria

getOrElse :: Maybe Int -> Int
getOrElse (Just n) = n
getOrElse Nothing = error "Segmentation Fault"

multiplicator = NBOp Mul (Var "x") (Var "y")
complejeitor = NBOp Mul (NBOp Add (Var "x") (NCte 2)) (Var "y")

mem1 :: Memoria
mem1 = Mem [("x",4),("y",5)]

nBinOpToOp :: NBinOp -> Int -> Int -> Int
nBinOpToOp Add = (+)
nBinOpToOp Sub = (-)
nBinOpToOp Mul = (*)
nBinOpToOp Div = div
nBinOpToOp Mod = mod
nBinOpToOp Pow = (^)

evalNExp :: NExp -> Memoria -> Int
evalNExp (NCte n) _ = n
evalNExp (Var a) memoria = getOrElse (cuantoVale a memoria)
evalNExp (NBOp nBinOp nExp1 nExp2) memoria = (nBinOpToOp nBinOp) (evalNExp nExp1 memoria) (evalNExp nExp2 memoria)

simplificaitor = NBOp Mul (NBOp Add (NCte 3) (NCte 2)) (NCte 4)

cfNExp :: NExp -> NExp
cfNExp (NCte n) = NCte n
cfNExp (Var a) = Var a
cfNExp (NBOp nBinOp nExp1 nExp2) = simpExp nBinOp (cfNExp nExp1) (cfNExp nExp2)

simpExp :: NBinOp -> NExp -> NExp -> NExp
simpExp nBinOp (NCte a) (NCte b) = NCte ((nBinOpToOp nBinOp) a b)
simpExp nBinOp nExp1 nExp2 = NBOp nBinOp nExp1 nExp2

-- Que lo demuestre El Diego o d10s que son la misma persona

-- Ejercicio 2

data BExp = BCte Bool | Not BExp | And BExp BExp | Or BExp BExp | ROp RelOp NExp NExp deriving Show
data RelOp = Eq | NEq | Gt | GEq | Lt | LEq deriving Show

relOpToOp :: RelOp -> Int -> Int -> Bool
relOpToOp Eq = (==)
relOpToOp NEq = (/=)
relOpToOp Gt = (>)
relOpToOp GEq = (>=)
relOpToOp Lt = (<)
relOpToOp LEq = (<=)

bexp1 = And (BCte False) (BCte False)
bexp2 = And (ROp Eq multiplicator multiplicator) (Or (BCte True) (BCte False))

evalBExp :: BExp -> Memoria -> Bool
evalBExp (BCte b) _ = b
evalBExp (Not bExp) memoria = not (evalBExp bExp memoria)
evalBExp (And bExp1 bExp2) memoria = (evalBExp bExp1 memoria) && (evalBExp bExp2 memoria)
evalBExp (Or bExp1 bExp2) memoria = (evalBExp bExp1 memoria) || (evalBExp bExp2 memoria)
evalBExp (ROp relOp nExp1 nExp2) memoria = (relOpToOp relOp) (evalNExp nExp1 memoria) (evalNExp nExp2 memoria)

cfBExp :: BExp -> BExp
cfBExp (BCte b) = BCte b
cfBExp (Not bExp) = simpBexp (Not (cfBExp bExp))
cfBExp (And bExp1 bExp2) = simpBexp (And (cfBExp bExp1) (cfBExp bExp2))
cfBExp (Or bExp1 bExp2) = simpBexp (Or (cfBExp bExp1) (cfBExp bExp2))
cfBExp (ROp relOp nExp1 nExp2) = ROp relOp (cfNExp nExp1) (cfNExp nExp2)


simpBexp :: BExp -> BExp 
simpBexp (Not (BCte b)) = BCte (not b)
simpBexp (And (BCte b1) (BCte b2)) = BCte (b1 && b2)
simpBexp (Or (BCte b1) (BCte b2)) = BCte (b1 || b2)
simpBexp bExp = bExp
