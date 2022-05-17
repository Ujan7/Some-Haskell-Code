--EJERCICIO 1:
data Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Eq, Ord, Show)

titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Computacion"
titulo Astronomia = "Licenciatura en Astronomia"

--EJERCICIO 2:
-- Agrego a mi tipo de dato Carrera "deriving Eq" para lograr que las constantes sean comparables.
-- Tambien, agrego  "ord" para que puedan ser ordenables.
-- Y agrego "show" para que pueda mostrar las constantes en pantalla.
-- Resumiendo : "Astronomia deriving (Eq, Ord, Show)"

--Ejercicio 3:
--a) Defino los tipos de los datos a usar:
type Ingreso = Int 

data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving Show

data Area = Administrativa | Ensenanza | Economia | Postgrado deriving Show

data Persona = Decane | Docente Cargo | Nodocente Area | Estudiante Carrera Ingreso deriving Show

--b) El tipo de constructor Docente es Docente :: Cargo -> Persona
--c) Primero defino una funcion auxiliar:
mismosCargos :: Cargo -> Cargo -> Bool
mismosCargos Titular Titular = True
mismosCargos Asociado Asociado = True
mismosCargos Adjunto Adjunto = True
mismosCargos Asistente Asistente = True
mismosCargos Auxiliar Auxiliar = True
mismosCargos _ _ = False

cuantos_doc :: [Persona] -> Cargo -> Int
cuantos_doc [] _ = 0
cuantos_doc ((Docente c1):xs) c2 | (mismosCargos c1 c2) = 1 + cuantos_doc xs c2
                                 | otherwise = cuantos_doc xs c2
cuantos_doc (_:xs) c2 = cuantos_doc xs c2

--d) De nuevo, defino una funcion auxiliar para usar el filter:
docycargos :: Cargo -> Persona -> Bool
docycargos c (Docente cx) = mismosCargos c cx
docycargos _ _ = False

cuantos_doc' :: [Persona] -> Cargo -> Int 
cuantos_doc' xs c = length (filter (docycargos c) xs)

--EJERCICIO 4:
--a)
primerElemento :: [a] -> Maybe a 
primerElemento [] = Nothing
primerElemento (x:xs) = Just x

--EJERCICIO 5:
data Cola = VaciaC | Encolada Persona Cola deriving Show
--a)
--1) 
atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada p cola) = Just cola

--2)
encolar :: Persona -> Cola -> Cola
encolar persona VaciaC = Encolada persona VaciaC
encolar persona (Encolada persona0 VaciaC) = Encolada persona0 (Encolada persona VaciaC)
encolar persona (Encolada persona0 (Encolada persona1 cola)) = Encolada persona0 (encolar persona (Encolada persona1 cola))

--3)
busca :: Cola -> Cargo -> Maybe Persona 
busca VaciaC c = Nothing
busca (Encolada(Docente cx) cola) c | (mismosCargos c cx) = Just (Docente cx)
                                    | otherwise = Nothing
busca (Encolada persona cola) cx = busca cola cx

--EJERCICIO 6:

data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b) deriving (Show)
--a)¿Como se debe instanciar el tipo ListaAsoc para representar la informacion almacenada
--en una guıa telefonica?
-- Como bien sabemos, una guia telefonica es una lista que contiene nombres y apellidos de las personas, a los cuales les corresponde un cierto numero telefonico.
-- Por ende, deberiamos tener en cuenta que al instanciar el tipo, deberiamos encontrar palabras (String) y numeros (Int)
type GuiaTelefonica = ListaAsoc String Int

--b)
--1)
la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo a b l) = 1 + la_long l

--2)
la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia Vacia = Vacia
la_concat l1 Vacia = l1
la_concat Vacia l2 = l2
la_concat (Nodo a b l) l2 = Nodo a b (la_concat l l2)


--3)
la_pares :: ListaAsoc a b -> [(a, b)]
la_pares Vacia = []
la_pares (Nodo a b l) = (a, b) : (la_pares l)

--4)
la_busca :: Eq a => a -> ListaAsoc a b -> Maybe b
la_busca _ Vacia = Nothing
la_busca x (Nodo a b y) | (x == a) = Just b
                        | otherwise = Nothing


--5)
la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar _ Vacia = Vacia
la_borrar d (Nodo a b l) | (d == a) = la_borrar d l
                         | otherwise = Nodo a b (la_borrar d l)
                    
--EJERCICIO 7:
data Arbol a = Hoja | Rama ( Arbol a ) a ( Arbol a ) deriving Show

--a)
a_long :: Arbol a -> Int
a_long Hoja = 0
a_long (Rama x y z) = 1 + a_long x + a_long z
--b)
a_hojas :: Arbol a -> Int
a_hojas Hoja = 1
a_hojas (Rama x y z) = a_hojas x + a_hojas z
--c)
a_inc :: Num a => Arbol a -> Arbol a 
a_inc Hoja = Hoja 
a_inc (Rama x y z) = Rama (a_inc x) (1 + y) (a_inc z)
--d)
a_map :: (a -> b) -> Arbol a -> Arbol b
a_map f Hoja = Hoja
a_map f (Rama x y z) = Rama (a_map f x) (f y) (a_map f z)

	