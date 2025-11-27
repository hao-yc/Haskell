--1. Tipus
-- data T = V

--Exercici: Definir el tipus Bit que permet representar un bit, amb valors Bit = {O, I}
data Bit = O | I
--Exercici: Definir la funció (.==.)::Bit -> Bit -> Bool que retorna True si i només si els dos bits són iguals.
(.==.) :: Bit -> Bit -> Bool
O .==. O = True   
I .==. I = True      
O .==. I = False   
I .==. O = False   

--1.2
-- Exercici: Definir el tipus DBit per a representar números d’exactament 2 Bits: data DBit = D ....
data DBit = D Bit Bit

--  Exercici: Definir la funció
dbitAint :: DBit -> Int
dbitAint (D h l) = 2*(valor h) + (valor l)
    where 
       valor I = 1
       valor O = 0

-- 1.3 Patrons
dbitAintP :: DBit -> Int
dbitAintP (D O O) = 0  -- Patrón: 00₂ = 0
dbitAintP (D O I) = 1  -- Patrón: 01₂ = 1  
dbitAintP (D I O) = 2  -- Patrón: 10₂ = 2
dbitAintP (D I I) = 3  -- Patrón: 11₂ = 3

-- 1.4 Definicions recursives
data Nat = Z | S Nat deriving Show  

--Ex1
natAint :: Nat->Int
natAint Z = 0
natAint (S xs) = 1 + natAint xs -- Cuenta cuántos S hay antes de llegar a Z

--Ex2
sumaNat :: Nat -> Nat -> Nat 
sumaNat Z xs = xs
sumaNat xs Z = xs
sumaNat (S xs) (xs2) = S (sumaNat xs xs2) -- Añade un S al resultado de sumar xs y xs2

--Ex3
data MInt = Zero | P Nat | N Nat deriving Show -- P Nat-->úmeros positivos, N Nat-->números negativos 


intAnat :: Int -> Nat -- funció per pasar de int a nat
intAnat 0 = Z 
intAnat 1 = (S Z)
intAnat x =  (S (intAnat (x-1))) -- n = 1 + (n-1)

--Ex4
mintAint :: MInt -> Int 
mintAint Zero = 0
mintAint (N xs) = (-natAint(xs)) --de nat a int y el nega
mintAint (P xs) = natAint(xs) -- de nat a int

intAmint :: Int -> MInt
intAmint 0 = Zero
intAmint xs
    |xs <0  = N (intAnat (abs xs)) -- convertir a negatiu
    |otherwise = P (intAnat xs) -- convertir a positiu

--Ex5 usando conversores
sumaMInt :: MInt -> MInt -> MInt
sumaMInt Zero xs = xs -- 0 + x = x
sumaMInt xs Zero = xs -- x + 0 = x
sumaMInt (P xs) (P ys)= P (sumaNat xs ys) -- a+b
sumaMInt (N xs) (N ys)= N (sumaNat xs ys) -- (-a)+(-b)
sumaMInt xs ys = intAmint(mintAint(xs) + mintAint(ys)) 

--Ex6 sin conversores
menorNat :: Nat -> Nat -> Bool
menorNat Z Z = False
menorNat Z _ = True
menorNat _ Z = False
menorNat (S x) (S y) = menorNat x y

restaNat :: Nat -> Nat -> Nat
restaNat x Z = x
restaNat Z _ = Z
restaNat (S x) (S y) = restaNat x y

sumaMIntD :: MInt -> MInt -> MInt
sumaMIntD Zero xs = xs
sumaMIntD xs Zero = xs
sumaMIntD (P xs) (P ys) = P (sumaNat xs ys)   -- (a+b)
sumaMIntD (N xs) (N ys) = N (sumaNat xs ys)   -- -(a+b)
sumaMIntD (P xs) (N ys)
  | menorNat xs ys = N (restaNat ys xs)
  | menorNat ys xs = P (restaNat xs ys)
  | otherwise      = Zero

sumaMIntD (N xs) (P ys)
  | menorNat xs ys = P (restaNat ys xs)
  | menorNat ys xs = N (restaNat xs ys)
  | otherwise      = Zero

--5 llistes 
data Llista a = B | L a ( Llista a) deriving Show

--Ex1
desdeL :: Llista a -> [a]
desdeL B = []
desdeL (L x xs) = x:desdeL xs  -- Lista con primer elem. x y el resto xs , devuelve una lista normal con x de primer elem. 
-- y restos conversion de los restos

aL :: [a]-> Llista a
aL []=B 
aL (x:xs)= L x (aL xs)

--Ex2.1
initL :: Int -> Llista Int
initL 0 = B 
initL x = L (x) (initL (x-1)) --Añade x al principio y sigue con x-1

--Ex2.2
giraL :: Llista a -> Llista a
giraL xs = giraLaux xs B -- Inicia el proceso
     where 
        giraLaux B aux = aux --Cuando ya no hay más elementos, devuelve el acumulador
        giraLaux (L x xs) aux = giraLaux xs (L x aux) --Mueve el primer elemento al principio del acumulador y sigue

--Ex2.3
initLdL :: Int -> Llista (Llista Int)
initLdL 0 = B 
initLdL n = L ( initL n) (initLdL (n-1)) --Una lista primer elemento es la lista initL n ,y resto son listas generadas a partir de n-1

desdeLdL :: Llista (Llista a) -> [[a]]
desdeLdL B = []
desdeLdL (L xs xss) = desdeL xs : desdeLdL xss --Convierte la primera sublista (xs) con desdeL y convirtiendo el resto (xss) 

--Ex2.4
aplastaL :: Llista (Llista a) -> Llista a
aplastaL B = B 
aplastaL  (L B ys)  = aplastaL ys --Si la primera sublista está vacía (B), la saltas y sigues con el resto (ys)
aplastaL (L  (L x xs) ys) = ( L x (aplastaL (L xs ys)))

--Ex3
mapejaL :: (a-> b)-> Llista a-> Llista b
mapejaL f B = B
mapejaL f (L x xs)= (L (f x) (mapejaL f xs)) --Aplica f al primer elemento, y sigue con el resto (recursión)

--Ex4
initLdL2 :: Int-> Llista (Llista Int)
initLdL2 n = mapejaL initL (aL [n,n-1..1]) -- Convierte [n, n-1 .. 1] a Llista y aplica initL a cada número

--Ex5
desdeLdL2::  Llista (Llista a)-> [[a]]
desdeLdL2 xs = desdeL (mapejaL desdeL xs) -- Aplica desdeL a cada sublista, luego convierte el resultado completo
