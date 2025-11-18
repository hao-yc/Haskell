--Ejercicio 2

-- Composición: (.)
-- Aplicación: ($)

(.$.) :: (a -> b) -> a -> b    -- aplicación
(.$.) f x = f x -- equivalente a $

(...) :: (b -> c) -> (a -> b) -> (a -> c)    -- composición
(...) f g = \x -> f (g x) -- dos funciones

-- Ejemplos de uso:
doble :: Num a => a -> a
doble x = x * 2

sucesor :: Num a => a -> a
sucesor x = x + 1

-- Uso de nuestras funciones redefinidas:
ejemplol = doble ... sucesor .$. 5    -- Equivale a doble(sucesor(5)) = 12
ejemplo2 = sucesor ... doble .$. 3    -- Equivale a sucesor(doble(3)) = 7

-- Comparación con las funciones estándar:
ejemplo1_std = doble . sucesor $ 5    -- Con funciones estándar
ejemplo2_std = sucesor . doble $ 3    -- Con funciones estándar