-- Ejercicio 1

udiv :: (Int, Int) -> Int
udiv (x,y) = div x y
-- Funcion Parcial => ejemplo udiv (1,0) = ⊥ 

udivE :: (Int, Int) -> Int
udivE (x,0) = error "No puedo dividir por 0"
udivE (x,y) = div x y
-- Funcion Parcial => ejemplo udivE (1,0) = ⊥ 

udivH :: (Int, Int) -> Int
udivH = uncurry div
-- Funcion Parcial => ejemplo udivH (1,0) = ⊥ 

succ :: Int -> Int
succ x = x + 1
-- Funcion Total => Nunca da ⊥ si recibe valores totalmente definidos

succH :: Int -> Int
succH = (+) 1
-- Funcion Total => Nunca da ⊥ si recibe valores totalmente definidos

porLaMitad :: Int -> Int
porLaMitad = flip div 2
-- Funcion Total => Nunca da ⊥ si recibe valores totalmente definidos

conDieresis :: Char -> Char
conDieresis 'u' = 'ü'
-- Funcion Parcial => ejemplo conDieresis 'e' = ⊥ 

conDieresisB :: Char -> Char
conDieresisB 'u' = 'ü'
conDieresisB c = conDieresisB c
-- Funcion Parcial => ejemplo conDieresisB 'e' = ⊥ 

conTildePM :: Char -> Char
conTildePM 'a' = 'á'
conTildePM 'e' = 'é'
conTildePM 'i' = 'í'
conTildePM 'o' = 'ó'
conTildePM 'u' = 'ú'
-- Funcion Parcial => ejemplo conDieresisB 'p' = ⊥ 

conTildeE :: Char -> Char
conTildeE c = if esVocal c
then conTildePM c
else error "El valor recibido no es vocal"
-- Funcion Parcial => ejemplo conTildeE 'p' = ⊥ 

esVocal :: Char -> Bool
esVocal x = True

esMinuscula :: Char -> Bool
esMinuscula = esVocal

conTilde :: Char -> Char
conTilde c = if esVocal c && esMinuscula c
then conTildePM c
else c
-- Funcion Total => Si asuminmos que esVocal y esMinuscula son totales, nunca da ⊥ si recibe valores totalmente definidos

-- Ejercicio 2

-- udiv == udivH == udivE
-- succ == succH
-- porLaMitad no tiene funciones equivalentes
-- conDieresis == conDieresisB
-- conTildePM == conTildeE
-- conTilde no tiene funciones equivalentes


-- Ejercicio 3

-- Con twice = \f -> \x -> f (f x)

-- a. twice doble
   -------------- por def twice

-- (\f -> \x -> f (f x)) doble
   --------------------------- beta reduccion, f = doble
--  (\x -> doble (doble x))


-- b. twice doble 2
   ----------------- por def twice
   
-- (\f -> \x -> f (f x)) doble 2
   --------------------------- beta reduccion, f = doble
--  (\x -> doble (doble x)) 2 
   --------------------------- beta reduccion, x = 2
--  doble (doble 2)
          ---------
    ---------------     
--  doble 2 + doble 2
    -------------------
--  2 + 2 + 2 + 2
    ------------- por matematicas
--  8

-- c. twice
      ----- def twice
-- (\f -> \x -> f (f x))




-- Ejercicio 4

-- Con twice = f = g
-- where g x = f (f x)

-- a. twice doble
   -------------- por def twice
-- g


-- b. twice doble 2
      -----------    por def twice
-- g 2
   --- def g, x = 2
--  doble (doble 2)
           -------- beta reduccion, x = 2
    ---------------     
--  doble 2 + (doble 2)
    -------------------
--  2 + 2 + 2 + 2
    ------------- por matematicas
--  8

-- c. twice




-- Ejercicio 5

-- Con twice f x = f (f x)

-- a. twice doble
-- no tiene redex

-- b. twice doble 2
      -----------    por def twice
-- doble (doble 2)
         -------- por def de doble
    ---------------     
--  doble 2 + (doble 2)
    -------------------
--  2 + 2 + 2 + 2
    ------------- por matematicas
--  8

-- c. twice
-- no tiene redex


-- Ejercicio 6 

-- a. a
-- bottom solo, y no es diferente de bottom
-- x = undefined
-- x = error ""

b. Int -> a
\x -> error ""

c. a -> b
\x -> error ""

