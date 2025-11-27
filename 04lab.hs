
import Data.Maybe
import Data.Foldable

class Iterador t where
    ele :: t a -> a           -- Retorna el primer element
    seg :: t a -> t a         -- Retorna la llista sense el primer element
    hasnext :: t a -> Bool    -- Comprova si queden elements per iterar

data Llista a = B |  L a (Llista a) deriving Show

instance Iterador Llista where
    -- ele: retorna el primer element
    ele :: Llista a -> a
    ele (L x _) = x
    
    -- seg: elimina el primer element
    seg :: Llista a -> Llista a
    seg (L _ xs) = xs
    
    -- hasnext: comprova si queden elements
    hasnext :: Llista a -> Bool
    hasnext B = False
    hasnext (L _ _) = True

sumElem::(Iterador t) => t Int-> Int
sumElem t = 
    if hasnext t  -- si hi ha elements
        then ele t + sumElem (seg t) -- suma elemAcutal + recursio de la resta
     else 0 

data Nat = Z | S Nat deriving Show

instance Eq Nat where
    (==):: Nat-> Nat-> Bool

    Z == Z = True 
    (S x) == (S y) = x == y
    _ == _ = False

instance Ord Nat where
    (<=) :: Nat -> Nat -> Bool
    Z <= _ = True
    (S _) <= Z = False
    (S x) <= (S y) = x <= y

instance Enum Nat where
    succ :: Nat -> Nat
    succ = S

    pred :: Nat -> Nat
    pred Z = Z
    pred (S x) = x  -- predecesor de (n+1) es n

    toEnum :: Int -> Nat
    toEnum n 
        | n <= 0 = Z
        | otherwise = S (toEnum (n - 1)) -- Si n > 0, devuelve 1 + toEnum(n-1)

    fromEnum :: Nat -> Int
    fromEn  um Z = 0
    fromEnum (S x) = 1 + fromEnum x -- (n+1) -> 1 + fromEnum(n)

    -- 2 Les classes Semigroup i Monoid
    -- (x <> y) <> z = x <> (y <> z)

    --  1- x <> y = y ++ x  --> z ++ (y ++ x)  //  (z ++ y) ++ x No es asociativa
    --  2- x <> y = x ++ y  --> (x ++ y) ++ z //  x ++ (y ++ z) Es asociativa
    --  3- x <> y = x : y --> Hay incompatibilidad de tipos debido a que (:) espera un elemento y una lista, no dos listas.

instance Semigroup Nat where
        (<>) :: Nat -> Nat -> Nat
        Z <> y = y          -- 0 + y = y
        (S x) <> y = S (x <> y)  -- (x+1) + y = (x + y) + 1

instance Monoid Nat where
    mempty = Z

-- comprobamos por ejemplo que --> tres == dos  False   , definiendo antes dos[ S (S Z) ] y tres [S (S (S Z))]

--3 La Clase Foldable
-- Definir la instancia de Foldable oer al tipus Llista

instance Foldable Llista where
    foldr _ z B = z
    foldr f z (L x xs) = f x (foldr f z xs) --funcio de detra a esquerra

    foldl _ z B = z
    foldl f z (L x xs) = foldl f (f z x) xs --funcio d'esquerra a dreta

    foldMap _ B = mempty
    foldMap f (L x xs) = f x <> foldMap f xs -- combina els resutats amb <>
-- Foldable no es pot definir per el tipus nat ja que no es un contenidor de valors, sino una representacio de nombres naturals.
-- Foldable funciona para:
-- [] :: * -> *    (contenedor)
-- Maybe :: * -> * (contenedor)
-- Llista :: * -> * (contenedor)

-- Pero Nat :: * (no es contenedor) 

--3.1 Exercicis amb llistes 

--3.1.1 
maxInt :: [Int] -> Int
maxInt [] = 0
maxInt (x:xs) = foldr max x xs

--3.1.2
numNothing :: [Maybe a] -> Int
numNothing = foldr (\x count -> if isNothing x then count + 1 else count) 0
-- Para cada elemento: si es Nothing, suma 1 al contador

--3.1.3
tuplaMaxMin :: [Int] -> (Int, Int)
tuplaMaxMin xs = (maxInt xs, foldr min (maxInt xs) xs)

--3.1.4
tuplaAux :: [Double] -> (Double, Double)
tuplaAux = foldr (\x (count, sum) -> (count + 1, sum + x)) (0, 0) -- Acumula (contador, suma_total)

mitjana :: [Double] -> Double
mitjana [] = 0
mitjana xs = let (count, total) = tuplaAux xs  --Llama a tuplaAux xs para obtener (count, total)
             in total / count

--3.1.5
totsMax :: [Int] -> [Int]
totsMax xs = 
    let maxVal = maxInt xs  -- Encuentra el valor maximo en la lista
    in foldr (\x a -> if x == maxVal then x : a else a) [] xs -- Construye una lista con todos los elementos que son iguales al máximo

--3.1.6
posicionsLletra :: Char -> String -> [Int]
posicionsLletra c str = 
    foldr (\(i, char) a -> if char == c then i : a else a) [] -- Construeix la llista d'indexs on la lletra coincideix
          (zip [0..] str) --[(0,'p'),(1,'a'),(2,'t'),(3,'a')...]

--3.1.7
sumPar :: [[Int]] -> [Int] -- Para cada sublista: calcula su suma y la anade
sumPar = foldr (\sublista a -> sum sublista : a) []

sumTot :: [[Int]] -> Int -- Suma todas las sumas de las sublistas
sumTot = foldr (\sublista a -> sum sublista + a) 0

--3.1.8
separar :: Ord a => [a] -> a -> ([a],[a])
separar lista valor= foldr (\x (xs,ys)-> 
    if x<=valor  then (x:xs,ys) --si x <= valor, a la primera llista
    else (xs, x:ys)) --si x <= valor, a la segona llista    
    ([],[]) lista -- Aculador de tuplas 

--3.1.9
ordenar :: Ord a => [a] -> [a]
ordenar (xs) = foldr (\x ys-> fst(separar ys x)++[x]++snd(separar ys x)) [] xs --

