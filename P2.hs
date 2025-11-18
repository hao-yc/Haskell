-- LAB2
-- Exercici 1
p = 2 
q = 1
dr= p/q
-- si incorporamos dr nos da que p q y dr son doubles
de= div (truncate p) (truncate q)
-- div necesita que los parametros sean integer entonces no nos copilara, por lo tanto necesitaremos usar el truncate

--Exercici 2
(.$.) :: (a -> b) -> a -> b   
(.$.) f x = f x 

(...) :: (b -> c) -> (a -> b) -> (a -> c)   
(...) f g = \x -> f (g x) 

-- Funcion per provar
doble :: Num a => a -> a
doble x = x * 2

sumaTres :: Num a => a -> a
sumaTres x = x + 3

-- Uso de nuestras funciones person:
ejemplol = doble ... sumaTres .$. 5   --16
ejemplo2 = sumaTres ... doble .$. 3   --9

-- Comparación con las funciones estándar:
ejemplo1_std = doble . sumaTres $ 5   --16
ejemplo2_std = sumaTres . doble $ 3    --9

arrodonir :: Double -> Integer -> Double
arrodonir valor numDec = -- si  3,1437 2
    let n= 10^numDec -- 100
    in (/n) ... fromIntegral ... round ... (*n) .$. valor --3,1437--> 314,37-->314-->314.0 --> 314.0/100 -->3,14

arrodonirb :: Double->Integer->Double
arrodonirb valor numDec = (fromIntegral . round $ valor*10^numDec)/10^numDec
--permet el no us dels parentesis perque tenen precedencies especifiques  mentres que les f. personalitzades tenen precedencia normal

-- Definir la funció elevar al quadrat: 
quad :: Int -> Int
quad x = x * x

-- Multiplicar dos enters
multiplicar :: Int -> Int -> Int
multiplicar x y = x * y

--Triple 
triple :: Int -> Int
triple x = x * 3

--Triple luego cuadrado 
triplequad :: Int -> Int
triplequad = quad . triple
-- fr = s.g == \x -> quad (triple x)

-- Multiplicar luego elevar al cuadrado
multiplicarquad :: Int -> Int -> Int
multiplicarquad x y = quad (multiplicar x y)

-- Ejemplos de uso:
--quad 5          -- 25
--multiplicar 4 6  -- 24
--triple 7         -- 21
--triplequad 4     -- 144
--multiplicarquad 3 5  -- 225

-- Exercici: Reescriure, usant composició, amb el mínim número de parèntesis:

inc x = x +1

f1 x = inc (inc . inc $ x + inc (x))
f2 x = ( inc . inc $ x ) + inc  (x + x)

(.>) :: a->(a->b)-> b
g .> f = f g

res:: Integer
res= inc 0 .> inc .> inc

--3 Recursivitat
--Ex1 División y Residuo
divisio :: Int->Int->Int  -- División entera por restas sucesivas
divisio x y
    | x < y = 0
    | otherwise = (divisio (x - y) y ) + 1 

residu :: Int-> Int-> Int -- Calcula el resto de división entera
residu x y 
    | x < y = x 
    | otherwise = ( residu (x - y) y)

--Ex2 Combinatoria
binomial:: Int->Int->Int -- Coeficiente binomial C(n,k) (combinaciones)
binomial n 0 = 1
binomial 0 k = 0
binomial n k = (binomial (n - 1) k ) + ( binomial (n - 1) (k - 1))  

--Ex3 Sumatoris
sumaN:: Int -> Int  --Suma los primeros n números naturales
sumaN n 
    | n == 1 = 1
    | otherwise = sumaN (n - 1) + n

sumaNPar:: Int-> Int -- Suma los primeros n números pares
sumaNPar n 
    | n == 1 = 2
    | otherwise = sumaNPar (n - 1) + 2*n 

--Ex4   
sumaG :: (Eq t, Num t, Num a) => (t -> a) -> t -> a --Sumatoria generalizada de f(1) a f(n)
sumaG f 0 = 0                       -- Suma vacía = 0
sumaG f n = f n + (sumaG f (n - 1))   -- Aplica f a cada término y suma

sumaNG :: (Eq a, Num a) => a -> a  -- Suma números naturales usando sumaG
sumaNG n = sumaG id n               -- Suma 1 + 2 + ... + n (id x = x)

doble :: (Num a)=>a -> a --Duplica un número (función auxiliar)
doble x= 2*x

sumaNParG :: (Eq a, Num a) => a -> a --Suma números pares usando sumaG
sumaNParG n = sumaG doble n         -- Suma 2 + 4 + ... + 2n

--Ex5 Producto de sumas
sumatoriD :: (Eq t, Num t, Num a) => (t -> a) -> (t -> a) -> t -> a --Producto de dos sumatorias diferentes
sumatoriD f g n = (sumaG f n) * (sumaG g n)  -- (Sumatorif(i)) × (Sumatorig(i))

--Ex6 -- Calcula derivada numérica con convergencia automática
drvd :: (Double -> Double) -> Double -> Double
drvd f p = aux 2 
    where 
      epsilon = 1e-6                -- Precisión deseada
      aux h =
        let next = (f (p + h) - f (p)) / h           -- Derivada con paso h
            small = (f (p + h/4) - f (p)) / (h/4)    -- Derivada con paso h/4
        in if (next - small) < epsilon
            then small              
            else aux (h/4)          