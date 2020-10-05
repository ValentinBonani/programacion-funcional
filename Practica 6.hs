doble x = 2 * x

-- Ejercicio 1

doble = \x -> 2 * x
-- Ppio de Ext
doble n = (\x -> 2 * x) n
-- Lado Izquierdo
doble n
-- Def doble
2 * n

-- Lado Derecho
(\x -> 2 * x) n
-- Beta reduccion, con x = n
2 * n


cuadruple x = 4 * x

compose doble doble = cuadruple
-- Ppio de Ext
compose doble doble n = cuadruple n
-- Lado Izquierdo
compose doble doble n
-- definicion de compose, con f = doble y g x = doble n
doble (doble n)
-- deficion de doble
doble (2 * n)
-- deficion de doble
2 * (2 * n)
-- aritmetica
4 * n
-- def cuadruple
cuadruple n

-- Ejercicio 2

-- a. para todo x. para todo y. x && y = not ((not x) || (not y))

x && y = not ((not x) || (not y))
-- 4 casos
-- CASO 1
-- x = True, y = True
-- Lado derecho
not ((not True) || (not True))
-- def not * 2
not (False || False)
-- def ||
not False
-- def not
True
-- def &&
True && True

-- CASO 2
-- x = False, y = True
-- Lado derecho
not ((not False) || (not True))
-- def not * 2
not (True || False)
-- def ||
not True
-- def not
False

-- Lado izquierdo
False && True
-- def &&
False

-- Caso 3
-- x = False, y = False
-- Lado derecho
not ((not False) || (not False))
-- def not * 2
not (True || True)
-- def ||
not True
-- def not
False

-- Lado izquierdo
False && False
-- def &&
False

-- Caso 4
-- x = True, y = False
-- Lado derecho
not ((not True) || (not False))
-- def not * 2
not (False || True)
-- def ||
not True
-- def not
False

-- Lado izquierdo
True && False
-- def &&
False

-- b. para todo x. para todo y. not (x || y) = not x && not y

not (x || y) == not x && not y
-- 4 Caminos

-- Caso 1 
-- x = False ; y = False
-- Lado derecho

not False && not False
-- Def de not * 2
True && True
-- Def &&
True

--Lado Izquierdo
not (False || False)
-- Def ||
not (False)
-- Def not
True

-- Caso 2 
-- x = False ; y = True
-- Lado derecho

not False && not True
-- Def de not * 2
True && False
-- Def &&
False

--Lado Izquierdo
not (False || True)
-- Def ||
not (True)
-- Def not
False

-- Caso 3
-- x = True ; y = False
-- Lado derecho

not True && not False
-- Def de not * 2
False && True
-- Def &&
False

--Lado Izquierdo
not (True || False)
-- Def ||
not (True)
-- Def not
False

-- Caso 4
-- x = True ; y = True
-- Lado derecho

not True && not True
-- Def de not * 2
False && False
-- Def &&
False

--Lado Izquierdo
not (True || True)
-- Def ||
not (True)
-- Def not
False

-- Ejercicio 3

suma' (x,y) = x + y
curry f x y = f (x,y)

suma x y = x + y
-- c. curry suma' = suma
-- Prpo Extencionalidad * 2
curry suma' x y = suma x y
-- Lado Izq
curry suma' x y
-- Def Curry, con f = suma', x = x, y = y
suma' (x,y)
-- Def suma'
x + y
-- Def suma
suma x y



uncurry f (x,y) = f x y
-- d. uncurry suma = suma'
-- Ppio de Ext UNA VEZ SOLA
uncurry suma (x,y) == suma' (x,y)
-- Lado Izq
uncurry suma (x,y)
-- Def uncurry, con f = suma, (x,y) = (x,y)
suma x y
-- def de suma
x + y
-- def suma' 
suma' (x,y)

-- Ejercicio 4

curry fst = const
-- Ppio de Ext x2
curry fst n m = const n m
-- Lado Izq
curry fst n m
-- Def de curry
fst (n,m)
-- Def de fst
n
-- Lado Der
const n m
-- Def de const
n

uncurry f (x,y) = f x y
flip f x y = f y x

uncurry (flip const) = snd
-- Ppio de Ext una vez
uncurry (flip const) (n,m) = snd (n,m)
-- Lado izquierdo
uncurry (flip const) (n,m)
-- Def uncurry, f = (flip const), (x,y) = (n,m)
flip const n m
-- Def flip, f = const, x = n, y = m
const m n
-- Def const 
m
-- Lado Derecho
snd (n,m)
-- Def snd
m

-- Ejercicio 5

curry f x y = f (x,y)
uncurry f (x,y) = f x y


a. para todo f. curry (uncurry f) = f 

curry (uncurry f) = f
-- Ppio de Ext x2
curry (uncurry f) x y = f x y 
-- Lado der
curry (uncurry f) x y 
-- Def de curry
uncurry f (x,y)
-- Def de uncurry
f x y

b. para todo f'. uncurry (curry f') = f'
-- Ppio de Ext una vez
uncurry (curry f') (x,y) = f' (x,y)
-- Lado izq
uncurry (curry f') (x,y)
-- Def uncurry
curry f' x y
-- Def curry
f' (x,y)

-- Ejercicio 6

assoc :: (a,(b,c)) -> ((a,b),c)
assoc (x,(y,z)) = ((x,y),z)

ppAssoc :: (((a,b),c) -> d) -> (a,(b,c)) -> d
appAssoc f p = f (assoc p)

appAssoc (uncurry (uncurry f)) = uncurry (compose uncurry f)
-- Ppio de ext 
appAssoc (uncurry (uncurry f)) (x,(y,z)) = uncurry (compose uncurry f) (x,(y,z))
-- lado izquierdo
appAssoc (uncurry (uncurry f)) (x,(y,z))
-- def de appasco
uncurry (uncurry f) (assoc (x,(y,z)))
-- def de assoc 
uncurry (uncurry f) ((x,y),z)
-- def de uncurry 
uncurry f (x,y) z
-- def de uncurry 
f x y z
-- Lado derecho
uncurry (compose uncurry f) (x,(y,z))
-- Def uncurry
compose uncurry f x (y,z)
-- Def compose
uncurry (f x) (y,z)
-- Def uncurry
f x y z

compose f g x = f (g x)

-- Ejercicio 7 

(f . g) x = f (g x)

cuadruple = doble . doble
doble = id . (*2)
    twice f = f . f

many f 0 = id 
many f n =  f . (many f (n - 1))


i. para todo f. para todo g. f . g = compose f g
-- ppio de ext
(f . g) x = compose f g x
--lado izq
(f . g) x 
-- def de .
f (g x)
--lado der
compose f g x
-- def de compose
f (g x)

swap (x,y) = (y,x)
swap . swap = id
-- ppio de ext
(swap . swap) (x,y) = id (x,y)
-- Lado izquierdo
(swap . swap) (x,y)
-- Def (.)
swap (swap (x,y))
-- Def swap de adentro
swap (y,x)
-- Def swap
(x,y)
-- Def id
id (x,y)

iii. para todo f. para todo g. para todo h.

f . (g . h) = (f . g) . h
-- Ppio de ext
f . (g . h) x = (f . g) . h x
-- lado izq
f . (g . h) x 
-- def . de mas a la izquierda
f ((g . h) x)
-- def de .
f (g (h x))
-- Lado der
(f . g) . h x
-- def . de mas a la derecha
(f . g) (h x)
-- def de .
f (g (h x))


curry f x y = f (x,y)


iv. curry . uncurry = id
-- Ppio de ext x3
(curry . uncurry) f x y = id f x y
--lado izq
(curry . uncurry) f x y
-- def .
curry (uncurry f) x y
-- def curry
uncurry f (x,y)
-- def uncurry 
f x y
-- lado der
f x y

assoc :: (a,(b,c)) -> ((a,b),c)
assoc (x,(y,z)) = ((x,y),z)

appAssoc :: (((a,b),c) -> d) -> (a,(b,c)) -> d
appAssoc f p = f (assoc p)

appAssoc f = f . assoc
-- Ppio ext una vez
appAssoc f (x,(y,z)) = (f . assoc) (x,(y,z))
-- Lado izquierdo
appAssoc f (x,(y,z))
-- Def appAssoc, f = f, p = (x,(y,z))
f (assoc (x,(y,z)))
-- Def de . 
 (f . assoc) (x,(y,z))

doble . doble = cuadruple
-- ya demostramos que doble . doble = cuadruple
doble . doble = doble . doble



ii. para todo f'. curry (uncurry (curry f')) = curry f'
-- ya demostramos que uncurry (curry f) = f
-- Entonces (uncurry (curry f') = f'
curry f' = curry f'


iii. para todo f. 
appAssoc (uncurry (uncurry f)) = (uncurry . uncurry) f . assoc
-- ya demostramos que appAssoc f = f . assoc
-- entonces siendo f = (uncurry (uncurry f)) -> appAssoc (uncurry (uncurry f)) = (uncurry (uncurry f)) . assoc
(uncurry (uncurry f)) . assoc = (uncurry . uncurry) f . assoc
-- ya demostramos que (f . g) x = f (g x) 
-- entonmces siendo f = uncurry, g = uncurry y x = f -> uncurry . uncurry f = uncurry (uncurry f)
(uncurry (uncurry f)) . assoc = (uncurry (uncurry f)) . assoc




iv. para todo f. 
(uncurry . uncurry) f . assoc = uncurry (uncurry . f) 
-- nosotros ya demostramos que appAssoc f = f . assoc
-- entonces siendo f = (uncurry . uncurry) f -> (uncurry . uncurry) f . assoc = appAssoc (uncurry . uncurry) f
appAssoc (uncurry . uncurry) f = uncurry (uncurry . f) 
-- nosotros ya demostramos que appAssoc f . g = compose f g
-- entonces siendo f = uncurry y g = f -> uncurry . g = compose uncurry f
appAssoc (uncurry . uncurry) f = uncurry (compose uncurry f) 

-- nostros ya demostramos que appAssoc (uncurry (uncurry f)) = uncurry (compose uncurry f) 
appAssoc (uncurry . uncurry) f = appAssoc (uncurry (uncurry f))

y estas son iguales

(uncurry . uncurry) appAssoc f


-- ya demostramos que uncurry (uncurry f) (assoc (x,(y,z))) = uncurry (compose uncurry f) (x,(y,z))

