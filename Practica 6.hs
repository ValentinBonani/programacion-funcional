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

