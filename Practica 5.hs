
-- Ejercicio 1

data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon
data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto Gusto

chocoHelate consH = consH Chocolate

{- Vasito :: Gusto -> Helado
Chocolate :: Gusto
Cucurucho :: Gusto -> Gusto -> Helado
Sambayón :: Gusto
Pote :: Gusto -> Gusto -> Gusto -> Helado
chocoHelate :: (Gusto -> a) -> a
g. chocoHelate Vasito :: Helado
h. chocoHelate Cucurucho :: Gusto -> Helado
i. chocoHelate (Cucurucho Sambayon) :: Helado
j. chocoHelate (chocoHelate Cucurucho) :: Helado
k. chocoHelate (Vasito DulceDeLeche) -- No tipa
l. chocoHelate Pote :: Gusto -> Gusto -> Helado
m. chocoHelate (chocoHelate (Pote Frutilla)) :: Helado -}


--Ejercicio 2

data DigBin = O | I deriving Show


{- a. dbAsInt :: DigBin -> Int, que dado un símbolo que representa un
dígito binario lo transforma en su significado como número. -}

dbAsInt :: DigBin -> Int
dbAsInt O = 0
dbAsInt I = 1

{- b. dbAsBool :: DigBin -> Bool, que dado un símbolo que representa un
dígito binario lo transforma en su significado como booleano. -}

dbAsBool :: DigBin -> Bool
dbAsBool O = False
dbAsBool I = True

{- c. dbOfBool :: Bool -> DigBin, que dado un booleano lo transforma en el
símbolo que representa a ese booleano. -}

dbOfBool :: Bool -> DigBin
dbOfBool False = O
dbOfBool True = I

{- d. negDB :: DigBin -> DigBin, que dado un dígito binario lo transforma en
el otro. -}

negDB :: DigBin -> DigBin
negDB O = I
negDB I = O

-- Ejercicio 3

data DigDec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving Show

{- ddAsInt :: DigDec -> Int, que dado un símbolo que representa un
dígito decimal lo transforma en su significado como número. -}

ddAsInt :: DigDec -> Int
ddAsInt D0 = 0
ddAsInt D1 = 1
ddAsInt D2 = 2
ddAsInt D3 = 3
ddAsInt D4 = 4
ddAsInt D5 = 5
ddAsInt D6 = 6
ddAsInt D7 = 7
ddAsInt D8 = 8
ddAsInt D9 = 9

{- b. ddOfInt :: Int -> DigDec, que dado un número entre 0 y 9 lo transforma
en el símbolo que representa a ese dígito. -}

ddOfInt :: Int -> DigDec
ddOfInt 0 = D0
ddOfInt 1 = D1
ddOfInt 2 = D2
ddOfInt 3 = D3
ddOfInt 4 = D4
ddOfInt 5 = D5
ddOfInt 6 = D6
ddOfInt 7 = D7
ddOfInt 8 = D8
ddOfInt 9 = D9

{- c. nextDD :: DigDec -> DigDec, que dado un dígito decimal lo transforma
en el siguiente según el orden circular dado en la definición. -}
nextDD :: DigDec -> DigDec
nextDD D0 = D1
nextDD D1 = D2
nextDD D2 = D3
nextDD D3 = D4
nextDD D4 = D5
nextDD D5 = D6
nextDD D6 = D7
nextDD D7 = D8
nextDD D8 = D9
nextDD D9 = D0

{- d. prevDD :: DigDec -> DigDec, que dado un dígito decimal lo transforma
en el anterior según el orden circular dado en la definición. -}

prevDD :: DigDec -> DigDec
prevDD D0 = D9
prevDD D1 = D0
prevDD D2 = D1
prevDD D3 = D2
prevDD D4 = D3
prevDD D5 = D4
prevDD D6 = D5
prevDD D7 = D6
prevDD D8 = D7
prevDD D9 = D8

-- Ejercicio 4

data Medida = Mm Float | Cm Float | Inch Float | Foot Float deriving Show

{- asMm :: Medida -> Medida, que dada una medida cualquiera la
transforma en una medida en milímetros que aproxima la dada según la
conversión establecida. -}
asMm :: Medida -> Medida
asMm (Mm num) = Mm num 
asMm (Cm num) = Mm (num * 10)
asMm (Inch num) = Mm (num * 25.4)
asMm (Foot num) = Mm (num * 304.8) 

{- b. asCm :: Medida -> Medida, que dada una medida cualquiera la
transforma en una medida en centímetros que aproxima la dada según la
conversión establecida. -}
asCm :: Medida -> Medida
asCm (Mm num) = Cm (num * 0.1)
asCm (Cm num) = Cm num
asCm (Inch num) = Cm (num * 2.54)
asCm (Foot num) = Cm (num * 30.48)

{- c. asInch :: Medida -> Medida, que dada una medida cualquiera la
transforma en una medida en pulgadas que aproxima la dada según la
conversión establecida. -}

asInch :: Medida -> Medida
asInch (Mm num) = Inch (num * 0.039)
asInch (Cm num) = Inch (num * 0.394)
asInch (Inch num) = Inch num 
asInch (Foot num) = Inch (num * 12)

{- d. asFoot :: Medida -> Medida, que dada una medida cualquiera la
transforma en una medida en pies que aproxima la dada según la conversión
establecida. -}

asFoot :: Medida -> Medida
asFoot (Mm num) = Foot (num * 0.003)
asFoot (Cm num) = Foot (num * 0.033)
asFoot (Inch num) = Foot (num * 0.83)
asFoot (Foot num) = Foot num

-- Ejercicio 5

data Shape = Circle Float | Rect Float Float

construyeShNormal :: (Float -> Shape) -> Shape
construyeShNormal c = c 1.0

{- a. uncurry Rect :: (Float, Foat) -> Shape -}
{- b. construyeShNormal (flip Rect 5.0) :: Shape -}
{- c. compose (uncurry Rect) swap :: (Float, Foat) -> Shape -}
{- d. uncurry Cucurucho :: (Gusto,Gusto) -> Helado -}
{- e. uncurry Rect swap :: NO TIPA -}
{- f. compose uncurry Pote :: Gusto -> (Gusto, Gusto) -> Helado -}
{- g. compose Just :: (a -> b) -> a -> Maybe b -}
{- h. compose uncurry (Pote Chocolate) ::  NO TIPA porque uncurry esta definido para 2 -}

{- Just :: a -> Maybe a
data Maybe a = Nothing | Just a deriving Show

Just 1 :: Maybe Int
Nothing :: Maybe Int

Just False :: Maybe Bool
Nothing :: Maybe Bool -}


-- Ejercicio 6

swap (a,b) = (b,a)
compose = (.)

e6a = uncurry Rect (1,2)
e6c = compose (uncurry Rect) swap (1,2)
e6d = uncurry Cucurucho (Chocolate,Chocolate)
e6f = compose uncurry Pote Chocolate
e6g = compose Just (+) 1

-- Ejercicio 7

data MayFail a = Raise Exception | Ok a
data Exception = DivByZero | NotFound | NullPointer | Other String
type ExHandler a = Exception -> a

{- tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b

tryCatch (Ok a) f g = f a 
tryCatch (Raise e) f g = g e

inversa 0 = Raise DivByZero
inversa x = Ok (1 / x)
res = tryCatch (Ok 1) (*2) (\e -> case e of DivByZero -> -1)
res2 = tryCatch (inversa 0) (*2) (\e -> case e of DivByZero -> -1) -}

tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b

tryCatch (Ok a) f g = f a 
tryCatch (Raise e) f g = g e

inversa :: Float -> MayFail Float
inversa 0 = Raise DivByZero
inversa x = Ok (1 / x)

tryCatch2 (Raise e) f = f e
tryCatch2 (Ok a) f = a  

inversaTrycatcheada x = tryCatch2 (inversa x) (\e -> case e of DivByZero -> 0)
res = tryCatch (Ok 1) (*2) (\e -> case e of DivByZero -> -1)
res2 = tryCatch (inversa 0) (*2) (\e -> case e of DivByZero -> -1)

data Either2 a b = Left2 b | Right2 a 

divSafe :: Int -> Int -> Either2 Int Exception
divSafe _ 0 = Left2 DivByZero
divSafe a b = Right2 (a * b)