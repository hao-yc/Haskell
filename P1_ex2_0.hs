-- 1. Entorno de treball
inc :: Double -> Double
inc i = i + 1

-- 2.0
primer :: a -> b -> a
primer x y = x
segon :: a -> b -> b
segon x y = y

-- 2.1
primer2_1 :: x -> y -> x
primer2_1 = \x -> (\y -> x)
segon2_1 :: x -> y -> y
segon2_1 = \x -> (\y -> y)

-- 2.3
r1 = primer (segon 1 2) 3 --2
r2 = segon (primer 1 2) 3 --3
r3 = primer (1 + segon 1 2 + 1) 2 --4
r4 = primer segon 1 2 3 -- 3

-- 2.4
(?) a b = a    -- `primer` 
(??) a b = b   -- `segon`

-- 2.5
inter :: (a -> b -> c) -> b -> a -> c
inter f x y = f y x --  inter primer 1 2 retorna 2

segonI = inter primer   -- segonI 1 2 retorna 2

e1 = (+) (inter (-) 1 2) (-1) --0
e2 = primer (primer (primer 0 0) 0) 0 --0
-- sense reparentitzar e3 = 0 + primer (segon 1 0) 1 , ens dona 0

e3 = 1 + primer (segon 1 0) 1 -- reparentitzar --> 1

e4 = div e1 e3 -- -->0, es poden reparentitzar e1 i/o e3

--funcio Equacio recta 
equacioRecta :: Double -> Double -> (Double -> Double)
equacioRecta m b = \x -> m * x + b

-- Recta y = 2x + 3
recta1 :: Double -> Double
recta1 = equacioRecta 2 3 -- recta1 x = 2*x + 3

-- Fixem pendent 4
rectapendent4 :: Double -> (Double -> Double)
rectapendent4 = equacioRecta 4 -- recta y = 4 * x + 1

rectaA = rectapendent4 0    -- y = 4x + 0 --> terminal --> rectaA 3 nos dara 12.0
rectaB = rectapendent4 5    -- y = 4x + 5

--3.Tipus
--Exercici: Definir el tipus M3 que conté tres valors 𝙼𝟹 = {𝙰, 𝙱, 𝙲}.
data M3 = A|B|C deriving (Show,Eq)
-- Ex2

-- La funcio cert tornara el primer valor
-- Ejemple cert True False = True
cert :: a -> b -> a
cert x y = x

fals :: a -> b -> b
fals x y = y

(.<.) :: M3 -> M3 -> Bool
(.<.) A B = cert True False    -- = True
(.<.) A C = cert True False    -- = True
(.<.) B C = cert True False    -- = True
(.<.) B A = fals True False    -- = False
(.<.) C A = fals True False    -- = False
(.<.) C B = fals True False    -- = False
(.<.) A A = fals True False    -- = False
(.<.) B B = fals True False    -- = False
(.<.) C C = fals True False    -- = False

-- 3.3
maxDos :: M3 -> M3 -> M3   -- He tenido que copilar todo al codigo en ghci antes de ejecutarlo
maxDos x y = if x .<. y then y else x -- si x es mayor que y devuelve y sino x

-- Per tres valor podria ser de la segunet manera
maxim :: M3 -> M3 -> M3 -> M3
maxim x y z = maxDos (maxDos x y) z

-- Versión con LET 
maximLet :: M3 -> M3 -> M3 -> M3
maximLet x y z = let maxXY = maxDos x y
                 in maxDos maxXY z

-- Versión con WHERE 
maximWhere :: M3 -> M3 -> M3 -> M3
maximWhere x y z = maxDos maxXY z
    where maxXY = maxDos x y -- se calcula primero 

-- El Let y el Where son eficazmente identicos.
--maximLet A B C == maximWhere A B C --> TRUE (he afegit la clase tipus Eq per poder compararles)