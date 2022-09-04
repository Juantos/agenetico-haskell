

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module AGenetico(
    combinacion1
    ,ejecutaCombinacion1
    ,combinacion2
    ,ejecutaCombinacion2
    ,combinacion2Aux
    ,combinacionCiclos
    ,ejecutaCombinacionCiclos
    ,combinacionCiclosAux
    ,mutacion1
    ,ejecutaMutacion1Int
    ,permutacioninter
    ,ejecutaPermutacionInter
    ,interludio
    ,permutacioninser
    ,ejecutaPermutacionInser
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
--combinacion1 xs ys i = take i xs ++ drop i ys

--Input: lista con todos los padres, tamaño de cromosoma y el porcentaje de veces que se ejecutará esta combinación del total de mutaciones/combinaciones
--       n es el número de veces que queremos ejecutar combinación 1
--Proceso: coge un padre aleatorio y lo mezcla con otro padre aleatorio cortando cada uno según una posición generada aleatoriamente
--Output: la combinación de un padre aleatorio con otro también aleatorio
combinacion1 :: [[a]] -> Int -> Int -> Int -> IO [[a]]
combinacion1 _ _ _ 0 = return [] 
combinacion1 padres tC porcentaje n = do
    rp1 <- randIntRango 0 porcentaje
    rp2 <- randIntRango 0 porcentaje
    pos <- randIntRango 0 (tC)
    let c = take pos (padres!!rp1) ++ drop pos (padres!!rp2)
    cs <- combinacion1 padres tC porcentaje (n-1)
    return (c:cs)

--Hace muchas combinaciones teniendo en cuenta el porcentaje (snd (mezcla!!1)) de los mejores padres especificados
ejecutaCombinacion1 :: [[a]] -> Int -> [(String,Int)] -> IO [[a]]
ejecutaCombinacion1 padres tC mezcla = do
    let porcentaje = ((snd (mezcla!!0))-1)
    comb1 <- combinacion1 padres tC porcentaje ((snd (mezcla!!1)))
    return comb1


    
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

--Input: lista con todos los padres y el porcentaje de veces que se ejecutará esta combinación del total de mutaciones/combinaciones
--       n es el número de veces que queremos ejecutar combinación2
--Proceso: coge un padre aleatorio y lo mezcla con otro padre aleatorio, alternando entre un gen de cada uno
--Output: la combinación de un padre aleatorio con otro también aleatorio

combinacion2Aux :: [a] -> [a] -> [a] -> Int -> [a]
combinacion2Aux [] [] zs _ =  zs
combinacion2Aux (x:xs) (y:ys) zs 0 = combinacion2Aux xs ys (zs++[x]) 1 
combinacion2Aux (x:xs) (y:ys) zs 1 = combinacion2Aux xs ys (zs++[y]) 0 

combinacion2 :: [[a]] -> Int -> Int -> IO [[a]]
combinacion2 _ _ 0 = return []
combinacion2 padres porcentaje n = do
    rp1 <- randIntRango 0 porcentaje
    rp2 <- randIntRango 0 porcentaje
    let c = combinacion2Aux (padres!!rp1) (padres!!rp2) [] 0
    cs <- combinacion2 padres porcentaje (n-1)
    return (c:cs)

ejecutaCombinacion2 :: [[a]] -> [(String, Int)] -> IO [[a]]
ejecutaCombinacion2 padres mezcla = do
    let porcentaje = (snd (mezcla!!0))-1
    comb2 <- combinacion2 padres porcentaje (snd (mezcla!!2))
    return comb2


--------------------------------------------------------------------------------------------------------------------------
combinacionCiclos :: Eq a => [a] -> [a] -> Int -> [a] --Recibe dos cromosomas y un número aleatorio y devuelve un cromosoma nuevo utilizando el cruce basado en ciclos (para cromosomas en los que los elementos no pueden repetirse)
combinacionCiclos xs ys i = combinacionCiclosAux xs ys (ciclo xs ys i []) []

ejecutaCombinacionCiclos :: Eq a => [[a]] -> Int -> [(String,Int)] -> IO [[a]]
ejecutaCombinacionCiclos padres tC mezcla = do
    rp1 <- randIntRango 0 ((snd (mezcla!!0))-1)
    rp2 <- randIntRango 0 ((snd (mezcla!!0))-1)
    rtc <- randIntRango 0 (tC)
    let combiCiclos = [combinacionCiclos (padres!!rp1) (padres!!rp2) rtc | x <- [1..(snd (mezcla!!3))]]
    return combiCiclos

combinacionCiclosAux :: Eq a => [a] -> [a] -> [a] -> [a] -> [a] --Recibe los mismos cromosomas que la función anterior + la lista con el ciclo a utilizar y una lista vacia para la recursion
combinacionCiclosAux [] [] ciclos zs = zs
combinacionCiclosAux (x:xs) (y:ys) ciclos zs 
    | elem x ciclos = combinacionCiclosAux xs ys ciclos (zs ++ [x])
    | otherwise = combinacionCiclosAux xs ys ciclos (zs ++ [y])

     

--MUTACIONES
--------------------------------------------------------------------------------------------------------------------------
mutacion1 :: [a] -> Int -> a -> [a]
mutacion1 xs pos a = (take pos xs) ++ [a] ++ (reverse (take ((length xs)-(pos+1)) (reverse xs)))

iteraMutacion1Int :: [[Int]] -> Int -> (Int, Int) -> Int -> Int -> IO [[Int]]
iteraMutacion1Int _ _ _ _ 0 = return []
iteraMutacion1Int padres tC valoresGenRange porcentaje n = do
    randomRange <- randIntRango (fst valoresGenRange) (snd valoresGenRange)
    rp <- randIntRango 0 porcentaje
    rtc <- randIntRango 0 tC
    let m = mutacion1 (padres!!rp) rtc randomRange
    ms <- iteraMutacion1Int padres tC valoresGenRange porcentaje (n-1)
    return (m:ms)

ejecutaMutacion1Int :: [[Int]] -> Int -> (Int,Int) -> [(String,Int)] -> IO [[Int]]
ejecutaMutacion1Int padres tC valoresGenRange mezcla = do
    let porcentaje = (snd (mezcla!!0))-1
    mut1 <- iteraMutacion1Int padres tC valoresGenRange porcentaje ((snd (mezcla!!4))-1)
    return mut1


--[mutacion1 (padres!!rp) rtc randomRange| x <- [1..((snd (mezcla!!4)))]]
--num es un numero aleatorio introducido como parametro de la función 
-------------------------------------------------------------------------------------------------------------------------
permutacioninter :: [a] -> Int -> Int -> [a]   --Recibe el cromosoma y las dos posiciones a intercambiar (importante introducir los numeros en orden)
permutacioninter xs pos1 pos2 = (take pos1 xs) ++ [xs !! pos2] ++ (interludio xs pos1 pos2) ++ [xs !! pos1] ++ (drop (pos2+1) xs)

ejecutaPermutacionInter :: [[a]] -> Int -> [(String, Int)] -> IO [[a]]
ejecutaPermutacionInter padres tC mezcla = do
    rp <- randIntRango 0 ((snd (mezcla!!0))-1)
    rtc1 <- randIntRango 0 (tC)
    rtc2 <- randIntRango 0 (tC)
    let inter = [permutacioninter (padres!!rp) rtc1 rtc2 | x <- [1..((snd (mezcla!!5)))]]
    return inter

--[permutacioninter (padres!!randomPadres) randomTamCromosoma randomTamCromosoma | x <- [1..((snd (porcentajeMezcla!!5)))]]

interludio :: [a] -> Int -> Int -> [a]
interludio xs pos1 pos2 = take (pos2-(pos1+1)) (drop (pos1+1) xs)
--------------------------------------------------------------------------------------------------------------------------
permutacioninser :: [a] -> Int -> Int -> [a]   --Recibe el cromosoma la posición donde insertar el gen y la posicion del gen a insertar
permutacioninser xs pos1 pos2 = (take (pos1+1) xs) ++ [xs !! pos2] ++ (drop (pos1+1) (deleteAt pos2 xs))

ejecutaPermutacionInser :: [[a]] -> Int -> [(String, Int)] -> IO [[a]]
ejecutaPermutacionInser padres tC mezcla = do
    rp <- randIntRango 0 ((snd (mezcla!!0))-1)
    rtc1 <- randIntRango 0 (tC)
    rtc2 <- randIntRango 0 (tC)
    let inser = [permutacioninser (padres!!rp) rtc1 rtc2 | x <- [1..((snd (mezcla!!6)))]]
    return inser

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
seleccionElitistaMaximizar xss it fitness = [fst (reverse (sortBy ordena listaTuplas) !! x) | x <- [1..it]] where
    listaTuplas = zip xss (map fitness xss)

seleccionElitistaMinimizar :: Eq a => [[a]] -> Int -> ([a] -> Double) -> [[a]] 
seleccionElitistaMinimizar xss it fitness = [fst ((sortBy ordena listaTuplas) !! x) | x <- [1..it]] where
    listaTuplas = zip xss (map fitness xss)
  
ordena :: ([a],Double) -> ([a],Double) -> Ordering
ordena (_,a) (_,b) | a<=b = LT
                   | otherwise = GT
