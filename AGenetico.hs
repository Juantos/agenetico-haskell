


{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module AGenetico(
    combinacion1
    ,combinacion2
    ,combinacion2Aux
    ,combinacionCiclos
    ,combinacionCiclosAux
    ,mutacion1
    ,permutacioninter
    ,interludio
    ,permutacioninser
    ,posEnLista
    ,ciclo
    ,seleccionRuleta
    ,seleccionRuletaAux
    ,seleccionElitistaMaximizar
    ,seleccionElitistaMinimizar
    ,ordena

) where
import Data.List
import System.Random
import System.IO.Unsafe
import Generador


--COMBINACIONES
--------------------------------------------------------------------------------------------------------------------------
combinacion1 :: [a] -> [a] -> Int -> [a] --Recibe dos cromosomas y la posicion a partir de la cual termina el primero y comienza el segundo
combinacion1 xs ys i = take i xs ++ drop i ys

{- combinacion1 :: [a] -> [a] -> Int -> [a] --Recibe dos cromosomas y la posicion a partir de la cual termina el primero y comienza el segundo
combinacion1 xs ys i = combinacionAux xs ys i []

combinacion1Aux :: [a] -> [a] -> Int -> [a] -> [a]
combinacion1Aux [x] [y] i zs
    | i==0 = zs ++ [y]
    | otherwise = zs ++ [x]
    
combinacionAux (x:xs) (y:ys) i zs 
    |i==0 = combinacion1Aux xs ys 0 (zs++[y])
    |otherwise = combinacion1Aux xs ys (i-1) (zs++[x])

-}
--------------------------------------------------------------------------------------------------------------------------
combinacion2 :: [a] -> [a] -> [a]     --Recibe dos cromosomas y genera uno nuevo que alterna un gen de cada uno de los padres
combinacion2 xs ys = combinacion2Aux xs ys [] 0

combinacion2Aux :: [a] -> [a] -> [a] -> Int -> [a]
combinacion2Aux [] [] zs _ =  zs
combinacion2Aux (x:xs) (y:ys) zs 0 = combinacion2Aux xs ys (zs++[x]) 1 
combinacion2Aux (x:xs) (y:ys) zs 1 = combinacion2Aux xs ys (zs++[y]) 0 


--------------------------------------------------------------------------------------------------------------------------
combinacionCiclos :: Eq a => [a] -> [a] -> Int -> [a] --Recibe dos cromosomas y un número aleatorio y devuelve un cromosoma nuevo utilizando el cruce basado en ciclos (para cromosomas en los que los elementos no pueden repetirse)
combinacionCiclos xs ys i = combinacionCiclosAux xs ys (ciclo xs ys i []) []

combinacionCiclosAux :: Eq a => [a] -> [a] -> [a] -> [a] -> [a] --Recibe los mismos cromosomas que la función anterior + la lista con el ciclo a utilizar y una lista vacia para la recursion
combinacionCiclosAux [] [] ciclos zs = zs
combinacionCiclosAux (x:xs) (y:ys) ciclos zs 
    | elem x ciclos = combinacionCiclosAux xs ys ciclos (zs ++ [x])
    | otherwise = combinacionCiclosAux xs ys ciclos (zs ++ [y])

     

--MUTACIONES
--------------------------------------------------------------------------------------------------------------------------
mutacion1 :: [a] -> Int -> a -> [a]
mutacion1 xs pos num = (take pos xs) ++ [num] ++ (reverse (take ((length xs)-(pos+1)) (reverse xs))) 
--num es un numero aleatorio introducido como parametro de la función 
--------------------------------------------------------------------------------------------------------------------------
permutacioninter :: [a] -> Int -> Int -> [a]   --Recibe el cromosoma y las dos posiciones a intercambiar (importante introducir los numeros en orden)
permutacioninter xs pos1 pos2 = (take pos1 xs) ++ [xs !! pos2] ++ (interludio xs pos1 pos2) ++ [xs !! pos1] ++ (drop (pos2+1) xs)

interludio :: [a] -> Int -> Int -> [a]
interludio xs pos1 pos2 = take (pos2-(pos1+1)) (drop (pos1+1) xs)
--------------------------------------------------------------------------------------------------------------------------
permutacioninser :: [a] -> Int -> Int -> [a]   --Recibe el cromosoma la posición donde insertar el gen y la posicion del gen a insertar
permutacioninser xs pos1 pos2 = (take (pos1+1) xs) ++ [xs !! pos2] ++ (drop (pos1+1) (deleteAt pos2 xs))

--------------------------------------------------------------------------------------------------------------------------
--permutacionmezcla :: [a] -> Int -> --Esta creo que mejor no hacerla porque tiene una componente aleatoria que tal y como es haskell veo complicada
--permutacionmezcla = 
--------------------------------------------------------------------------------------------------------------------------

--FUNCIONES AUXILIARES
posEnLista :: Eq a => a -> [a] -> Int --Funcion auxiliar que recibe un elemento y una lista que lo contiene y devuelve la posición que ocupa dicho elemento en la lista
posEnLista e xs = 
    if (elem e xs) == False 
        then error "El elemento no se encuentra en la lista"
    else length (takeWhile (/=e) xs)
--------------------------------------------------------------------------------------------------------------------------    
ciclo :: Eq a => [a] -> [a] -> Int -> [a] -> [a] --Función auxiliar que recibe dos cromosomas , un entero y una lista vacía en principio y devuelve un ciclo del primero comenzando en la posición introducida
ciclo xs ys pos zs = if elem (xs!!pos) zs then zs
    else ciclo xs ys (posEnLista (xs!!pos) ys) (zs++[xs!!pos])

generaNumAleatorioRango _ _ = 2.1

seleccionRuleta :: Eq a => [[a]] -> Int -> ([a] -> Double) -> [[a]] --Este metodo de selección debe utilizarse en funciones de maximización
seleccionRuleta xss it fitness = [seleccionRuletaAux listaTuplas (generaNumAleatorioRango (0) (sum (map fitness xss))) 0.0 | x <- [1..it]] where
    listaTuplas = zip xss (map fitness xss)

seleccionRuletaAux :: Eq a => [([a],Double)] -> Double -> Double -> [a]
seleccionRuletaAux (xs:xss) num acum =
    if (num > (acum+(snd xs))) 
        then seleccionRuletaAux  xss num (acum + (snd xs))
    else fst xs

                                                                    
seleccionElitistaMaximizar :: Eq a => [[a]] -> Int -> ([a] -> Double) -> [[a]] --Recibe la poblacion de la iteracion anterior, el número de individuos a seleccionar la funcion fitness y devuelve los individuos seleccionados 
seleccionElitistaMaximizar xss it fitness = [fst ((sortBy ordena listaTuplas) !! x) | x <- [it..(length xss)]] where
    listaTuplas = zip xss (map fitness xss)

seleccionElitistaMinimizar :: Eq a => [[a]] -> Int -> ([a] -> Double) -> [[a]] 
seleccionElitistaMinimizar xss it fitness = [fst ((sortBy ordena listaTuplas) !! x) | x <- [1..it]] where
    listaTuplas = zip xss (map fitness xss)
  
ordena :: ([a],Double) -> ([a],Double) -> Ordering
ordena (_,a) (_,b) | a<=b = LT
                   | otherwise = GT

