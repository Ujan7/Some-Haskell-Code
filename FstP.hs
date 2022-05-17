--PROYECTO 1 DE ALGORITMOS Y ESTRUCTURAS DE DATOS 1:

--EJERCICIO 1:
--a)
esCero :: Int -> Bool
esCero x | (x == 0) = True
         | (x /= 0) = False
--b)
esPositivo :: Int -> Bool
esPositivo x | (x > 0) = True
             | (x < 0) = False
--c)
esVocal :: Char -> Bool
esVocal v | ((v == 'a') || (v =='e') || (v == 'i') || (v == 'o') || (v == 'u')) = True
          | otherwise = False

--EJERCICIO 2:
--a) 
paraTodo :: [Bool] -> Bool
paraTodo [] = True
paraTodo (x:xs) = x == True && paraTodo xs

--b)
sumatoria :: [Int] -> Int 
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--c) 
productoria :: [Int] -> Int 
productoria [] = 1
productoria (d:xs) = d * productoria xs

--d)
factorial :: Int -> Int
factorial m | (m == 0) = 1
            | (m > 0) = m * factorial (m-1)

--e) 
promedio :: [Int] -> Int 
promedio [] = 0
promedio xs = div (sumatoria xs) (length xs)
	              
--EJERCICIO 3:

pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x:xs) = (n == x) || (pertenece n xs)

--EJERCICIO 4:

--a)
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] t = True
paratodo' (x:xs) t = (t x) && (paratodo' xs t)

--b)
existe' ::  [a] -> (a -> Bool) -> Bool
existe' [] t = False
existe' (x:xs) t = (t x) || (existe' xs t)

--c)
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (x:xs) t = (t x) + (sumatoria' xs t)

--d)
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] t = 1
productoria' (x:xs) t = (t x) * (productoria' xs t)

--EJERCICIO 5:
paratodo'' :: [Bool] -> Bool
paratodo'' xs = paratodo' xs id

--EJERCICIO 6:
--a)
todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs even
--b)
hayMultiplo :: Int -> [Int] -> Bool 
hayMultiplo n xs = existe' xs (\x -> x `mod` n == 0)
--c)
sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [0..n] (^2)
--d)
g :: Int -> Int 
g x | (x `mod` 2 == 0) = x
    | otherwise = 1 --ACLARACION: devuelvo 1 si x no es par debido al elemento neutro en la productoria, para que no me anule el valor.

multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria' xs g

--e)

--EJERCICIO 7:

--FUNCION MAP: map toma una función y una lista y aplica esa función a cada elemento de esa lista, produciendo una nueva lista.
--map f [] = []
--map f (x:xs) = f x : map f xs

--FUNCION FILTER: filter es una función que toma un predicado y una lista y luego devuelve la lista de elementos que satisfacen el predicado .
--filter p [] = []
--filter p (x:xs) | p x = x : filter p xs
--                | otherwise = filter p xs

--map succ [1, -4, 6, 2, -8] : En este caso, le estamos aplicando succ a cada elemento de nuestra lista xs [1, -4, 6, 2, -8].
--succ lo que hace es sumar uno a cada elemento, por lo que nos devolvera una lista con nuevos elementos, que serian los sucesores.
--Por tanto, nuestra nueva lista seria, [2,-3,7,3,-7].

--filter esPositivo [1, -4, 6, 2, -8] : Esta vez, estamos filtrando la lista mediante el parametro esPositivo, entonces filtraremos los valores positivos de nuestra lista xs.
--Por tanto, nuestra nueva lista seria [1,6,2]

--EJERCICIO 8:

duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = (2 * x) : duplica xs
--Con map:
duplicaM xs = map (*2) xs

--EJERCICIO 9

pares1 :: [Int] -> [Int]
pares1 [] = []
pares1 (x:xs) | ((x `mod` 2) == 0) = x: pares1 xs
              | otherwise = pares1 xs
--Con filter:
par n = (n `mod` 2) == 0
--Entonces:
pares1F xs = filter par xs

--EJERCICIO 10:

--a)
primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA f [] = []
primIgualesA f (x:xs) | (f == x) = f : primIgualesA f xs
                      | otherwise = []

--b) 
primIgualesA' :: Eq a => a -> [a] -> [a]
primIgualesA' f xs = takeWhile (==f) xs 

--EJERCICIO 11:

--a)
primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales (x:xs) | (x == head xs) = x : primIguales xs
                   | (x /= head xs) = [x]

--b)
primIgualesT :: Eq a => [a] -> [a]
primIgualesT xs = primIgualesA' (head xs) xs

-- (*)12
-- a) f :: (a, b) -> ... Esto esta bien tipado, ya que podemos nombrar a la tupla como x.
-- f x = ... Ademas, si cubre a todos los casos en que le ingremos una tupla de valores. 

-- b) 
--f :: (a, b) -> ... Bien tipado, ya que a la variable a matchea con x y analogamente b con y.
--f (x , y) = ... 

--c) f :: [(a, b)] -> ... Esto estaria bien definido. 
--   f (a , b) = ... Pero aca hay un problema de tipado, seria ((a,b):xs) ya que el elemento es una lista de tupla.

--d) f :: [(a, b)] -> ... Esta mal tipado, una lista de tuplas no es lo mismo que un elemento pegado a una lista ((x:xs)).
--   f (x:xs) = ...

--e) f :: [(a, b)] -> ...
--   f ((x, y) : ((a, b) : xs)) = ... Esta mal tipado, define primero una lista de tuplas, y luego define una tupla pegada a una lista de tuplas.

--f) f ) f :: [(Int, a)] -> ... Mal tipado.
--       f [(0, a)] = ...

--g) g) f :: [(Int, a)] -> ... Mal tipado, no coincide la especificacion, con una tupla de x y un entero.
--      f ((x, 1) : xs) = ...

--h) f :: [(Int, a)] -> ... Bien tipado.
--   f ((1, x) : xs) = ...

--i) f :: (Int -> Int) -> Int -> ... Esta mal tipado, la especificacion no cunbre todas las variables.
--   f a b = ...

--j) f :: (Int -> Int) -> Int -> ... Mal tipado.
--   f a 3 = ...

--k) f :: (Int -> Int) -> Int -> ... Esto esta bien tipado. Cubre con las variables de la especificacion.
--   f 0 1 2 = ...

--l) f :: a -> (a -> a) -> ... Mal tipado.
--   f a g = ...

--EJERCICIO 13:

--a) f :: (a, b) -> b
--   f (x,y) = y 

--b) f :: (a, b) -> c 
--   Esto no es correcto,no se puede crear un "c" a partir de un a y un b.

--c) f :: a -> b
--   f x = y

--d) f :: (a -> b) -> a -> b
--   f (x,y) = (x, y)

--e) f :: (a -> b) -> [a] -> [b]
--   f (x,y) = ((x:xs),(y:ys))

--f) f :: (a -> b) -> a -> c
--   No es correcto.

--g)  f :: (a -> b) -> (b -> c) -> a -> c
--    f (x,y,z) = (x,z)