--La intención de este documento y sus ejercicios es repasar brevemente algunos
--conceptos del paradigma funcional (en haskell) que sean útiles para luego
--realizar el lab1 tales como funciones recursivas, declaración de tipos, 
--alto orden, polimorfismo, etc.

--Comenzamos repasando la estructura presente en la definición de una función
--recursiva:

factorial :: Int -> Int              --signatura de la función (declara cantidad
                                     --y tipo de cada input/output de la
                                     --función)
factorial 0 = 1                      --case base (pattern-matching con "0")
factorial n = n * factorial (n-1)    --caso recursivo (pattern-matching con "n!=0")


--Repasemos funciones recursivas sobre listas.  Podemos caracterizar (al menos)
--tres tipos de funciones recursivas sobre listas: "MAP", "FILTER", "FOLD".

--Funciones recursivas del tipo "MAP":
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x : xs) = (2*x) : duplica xs

--Las funciones del tipo "map" consisten en aplicar una función concreta a cada 
--elemento de la lista, en el ejemplo de la función "duplica", la función sería 
--f(x) = 2*x

--Otro ejemplo de función de tipo "map" es:
mas1 :: [Int] -> [Int]
mas1 [] = []
mas1 (x : xs) = (x+1) : mas1 xs

--Parece que "duplica" y "mas1" son funciones muy similares.
--Solo se diferencian en la función que aplican a cada elemento de la lista,
--mientras una aplica 2*x, la otra aplica x+1.
--Por lo tanto, surge la pregunta si podemos generalizar las funciones del tipo
--"map", de forma tal que, "duplica" y "mas1" (e inclusive cualquier otra) sean
--un caso particular de dicha generalizacion.

--Para lograr esto utilizaremos el concepto de "ALTO ORDEN" que consiste en 
--permitir que una función pueda tomar como argumento de entrada o de salida una 
--función propiamente dicha.
--Es decir, una función es un valor posible de nuestro lenguaje tan naturalmente 
--como lo es un Int, o un Float, o un String, etc. 
 
--Entonces, la función que generaliza puede ser definida tomando como argumento  
--la función que es aplicada a cada elemento de la lista. 

generalMap:: [Int] -> (Int -> Int) ->  [Int]
generalMap [] _ = []
generalMap (x:xs) f = f x : generalMap xs f

--Notar que "generalMap" claramente utiliza alto orden.
--Toma una lista de enteros como primer argumento,
--una función como segundo argumento (que toma un entero y devuelve un entero) 
--y devuelve como output una lista de enteros.

--Más aún, podemos generalizar el tipo de la lista [Int] a través del 
--"POLIMORFISMO", que consiste en definir funciones que estén bien definidas
--para múltiples tipos de datos.
--Asi podemos dar una versión polimórfica de la función "generalMap" de la
--siguiente manera:

polGeneralMap :: [a] -> (a -> a) -> [a]
polGeneralMap [] _ = []
polGeneralMap (x:xs) f = f x : polGeneralMap xs f

--De esta manera, podemos utilizar "polGeneralMap" para realizar cualquier mapeo
--independientemente del tipo de la lista.
--De hecho, podemos dar una versión aún más polimórfica observando que el 
--dominio y la imagen de la función de mapeo no necesariamente deben ser del 
--mismo tipo. Es decir:

morePolGeneralMap :: [a] -> (a -> b) -> [b]
morePolGeneralMap [] _ = []
morePolGeneralMap (x:xs) f = f x : morePolGeneralMap xs f


--EJERCICIO 1: 
--a) redefinir la función "duplica" y "mas1" en términos de "generalMap" y 
--"polGeneralMap".
generalDuplica :: [Int] -> [Int]
generalDuplica xs = generalMap xs (*2)

polGeneralDuplica :: [Int] -> [Int]
polGeneralDuplica xs = polGeneralMap xs (*2)

generalMas1 :: [Int] -> [Int]
generalMas1 xs = generalMap xs (+1)

polGeneralMas1 :: [Int] -> [Int]
polGeneralMas1 xs = polGeneralMap xs (+1)

--b) definir la función esPar:: [Int] -> [Bool] en términos de "morePolGeneralMap". 
--Donde la función "esPar" mapea cada elemento de la lista a un booleano que 
--indica si el mismo es un número par. 
--Por ejemplo, esPar [2,9,4,5] = [True,False,True,False]. 
esPar :: [Int] -> [Bool]
esPar xs = morePolGeneralMap xs (even)

--Funciones recursivas del tipo "FILTER":
soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) | mod x 2 == 0 = x : soloPares xs
                 | mod x 2 /= 0 = soloPares xs
                 
--Las funciones del tipo "FILTER" consisten en filtrar los elementos de una 
--lista sujeto al cumplimiento de una condición booleana, en este ejemplo, la 
--condición sería "x es un número par".


--EJERCICIO 2: 
--a) generalizar la funciones de tipo "FILTER" sobre lista de enteros.
generalFilter :: (Int -> Bool) -> [Int] -> [Int]
generalFilter _ [] = []
generalFilter f (x:xs) | f x       = x : generalFilter f xs
                       | otherwise = generalFilter f xs

--b) dar una versión polimórfica de la misma.
polGeneralFilter :: (a -> Bool) ->  [a] -> [a]
polGeneralFilter _ [] = []
polGeneralFilter f (x:xs) | f x       = x : polGeneralFilter f xs
                          | otherwise = polGeneralFilter f xs

--c) redefinir la función "soloPares" en términos de dicha generalización.
generalSoloPares :: [Int] -> [Int]
generalSoloPares xs = generalFilter even xs         

--Funciones recursivas del tipo "FOLD":
sumatoria :: [Int] -> Int          
sumatoria [] = 0                  
sumatoria (x:xs) = x + sumatoria (xs)   
                 
--Las funciones del tipo "fold" se caracterizan por calcular un valor (vía una 
--función) en base a todos los elementos de una lista, en este ejemplo, la 
--función sería f(x,y) = x + y.


--EJERCICIO 3: 
--a) generalizar la funciones de tipo "FOLD" sobre lista de enteros.
generalFold :: [Int] -> (Int -> Int -> Int) -> Int -> Int
generalFold [] _ acc = acc
generalFold (x:xs) f acc = generalFold xs f (f acc x)

--b) dar una versión polimorfica de la misma.
polGeneralFold :: [a] -> (b -> a -> b) -> b -> b
polGeneralFold [] _ acc = acc
polGeneralFold (x:xs) f acc = polGeneralFold xs f (f acc x)
{- donde `a` se corresponde al tipo de los elementos de la lista, y `b` al tipo 
del acumulador en cuestión -}

--c) redefinir la función "sumatoria" en términos de dicha generalización.
generalSumatoria :: [Int] -> Int
generalSumatoria xs = generalFold xs (+) 0

polGeneralSumatoria :: [Int] -> Int
polGeneralSumatoria xs = polGeneralFold xs (+) 0

--Otro concepto interesante del paradigma funcional es que podemos definir 
--nuestos propios tipos:

type Radio = Float   --Define un "alias de tipo" (sinónimo)
type Lado = Float

--Vamos a definir 4 figuras
data Figura = Circulo Radio        --Cada uno de estos es un _constructor_
            | Cuadrado Lado        --define el constructor de un "Cuadrado"
            | Rectangulo Lado Lado --define el constructor de un "Rectangulo"
            | Punto                --define el constructor de un "Punto"
              deriving (Eq, Show)
              
--(esta última línea permite hacer que se impriman en pantalla los constructores
--de una Figura, y que se puedan comparar.)

--Y obviamente, podemos definir funciones sobre nuestros propios tipos de datos:

perimetro :: Figura -> Float
perimetro (Circulo radio) = 2 * pi * radio
perimetro (Cuadrado lado) = 4 * lado
perimetro (Rectangulo ancho alto) = 2 * ancho + 2 * alto
perimetro (Punto) = error "no se puede calcular el perimetro del punto"


--EJERCICIO 4: definir una función que devuelva la superficie de una "Figura"
superficie :: Figura -> Float
superficie (Circulo radio) = pi * radio * radio
superficie (Cuadrado lado) = lado * lado
superficie (Rectangulo ancho alto) = ancho * alto
superficie (Punto) = error "no se puede calcular la superficie del punto"


--EJERCICIO 5:
--a) Así como definimos el tipo "Figura" en el ejercicio anterior, ahora queremos 
--definir un tipo "Expr" que permita representar una expresión aritmética sobre 
--números enteros con nuestros propios constructores: suma, producto, resta, 
--división. 
--De esta manera, por ejemplo, la expresión "Resta (Producto (Suma 5 3) 2) 6" es 
--una "Expr" válida. Definir el tipo "Expr".
data Expr = Suma Expr Expr
          | Producto Expr Expr
          | Resta Expr Expr
          | Division Expr Expr
          | Numero Int
          deriving (Eq, Show)

--b) Luego, definir la semántica del tipo "Expr", i.e., definir una función que 
--evalúa (en forma natural) una expresión aritmética "Expr". Por ejemplo: 
--evaluar (Resta (Producto (Suma 5 3) 2) 6) = 10
evaluar :: Expr -> Int
evaluar (Numero n) = n
evaluar (Suma s1 s2) = evaluar (s1) + evaluar (s2)
evaluar (Producto p1 p2) = evaluar (p1) * evaluar (p2)
evaluar (Resta r1 r2) = evaluar (r1) - evaluar (r2)
evaluar (Division d1 d2) | d2 == (Numero 0) = error "no es posible dividir entre 0"
                         | otherwise        = evaluar (d1) `div` evaluar (d2)

--En base al tipo Expr y la definición de la función `evaluar`, el ejemplo 
--proporcionado en la consigna debe modificarse un poco...
exprEjemplo :: Expr
exprEjemplo = Resta (Producto (Suma (Numero 5) (Numero 3)) (Numero 2)) (Numero 6)

-- Luego para probar que la expresión termina dando 10, utilizar
-- ghci> evaluar exprEjemplo 


--EJERCICIO 6:

--a) Definir un tipo "BinTree" que permita representar un arbol binario
--polimórfico (i.e, en cuyos nodos se almacenen valores de tipo genérico "a").
data BinTree a = Nodo (BinTree a) a (BinTree a)   --nodo con dos subárboles
               | Hoja                             --nodo vacío
               deriving (Eq, Show)

ejemploArbol :: BinTree Int
ejemploArbol = Nodo (Nodo (Nodo Hoja 3 Hoja) 5 (Nodo Hoja 7 Hoja)) 
                    10 (Nodo Hoja 15 (Nodo Hoja 20 Hoja))

--b) Definir una función recursiva que devuelva la profundidad de un "BinTree".
profundidad :: BinTree a -> Int
profundidad Hoja = 0
profundidad (Nodo izq _ der) = 1 + profundidad(izq) `max` profundidad (der)  
--para que busque el camino más largo del árbol desde la raíz hasta alguna hoja

--c) Definir una función general de fold que opera sobre un "BinTree" y luego, 
--redefinir la función de profundidad del ítem b en términos de esta última. 
generalFoldBinTree ::  (b -> a -> b -> b) -> b -> BinTree a -> b
generalFoldBinTree _ acc Hoja = acc
generalFoldBinTree f acc (Nodo izq n der) = f (generalFoldBinTree f acc izq) n (generalFoldBinTree f acc der)

generalProfundidad :: BinTree a -> Int
generalProfundidad arbol = generalFoldBinTree (\acc_izq _ acc_der -> 1 + acc_izq `max` acc_der) 0 arbol

--d) Definir una función recuriva que devuelve una lista con los elementos del 
--BinTree, y luego redefinirla en términos de la función fold del ítem c.
elementos :: BinTree a -> [a]
elementos Hoja = []
elementos (Nodo izq n der) = elementos (izq) ++ [n] ++ elementos (der)

generalElementos :: BinTree a -> [a]
generalElementos arbol = generalFoldBinTree (\elem_izq n elem_der -> elem_izq ++ [n] ++ elem_der) [] arbol