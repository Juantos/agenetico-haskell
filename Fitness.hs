module Fitness(
    valoresViajante
    ,fitnessViajante
    ,distanciaManhattan
    ,fitnessCuadradoMagico
    ,fitnessCuadradoMagicoAux
    ,resta15
    ,fitnessSudoku
    ,fitnessSudokuAux
    ,cuentaIguales
    ,igualesAux
    ,borra
    ,decodificaCuadradoMagico
    ,decodificaSudoku
    ,decodificaViajante
    ,mejorMax
    ,mejorMin
    ,fitnessMochila
    ,fitnessMochilaAux
    ,mod2
    ,decodificaMochila

) where

import VariacionesCMS
import Data.Matrix as M
import Data.Vector as V
import Data.Vector.Mutable





--FITNESS VIAJANTE

--input: Numero entero que representa el número de puntos por lado del cuadrado
--output: lista de tuplas que representan cada punto del cuadrado
valoresViajante :: Int -> [(Int,Int)] --Coordenadas que utilizaremos para el problema del viajante (para asegurar que el camino mínimo sea 4n)
valoresViajante n = [(x,y) | x <- [0..n] , y <- [0..n] , (x==0 || y==0 || x==n || y==n)]


--input: Cromosoma de números enteros (hacen referencia a la lista de puntos obtenida en valoresViajante)
--output: Suma acumulada de las distancias entre los puntos de valores Viajante a los que hace referencia el cromosoma 
fitnessViajante :: [Int] -> Double 
fitnessViajante xs =  Prelude.sum ([distanciaManhattan ((valoresViajante 5)!!(xs!!x)) ((valoresViajante 5)!!(xs!!(x+1))) | x <- [0..((Prelude.length xs)-2)]]  
                        Prelude.++  [distanciaManhattan ((valoresViajante 5) !! 0) ((valoresViajante 5) !! 19)])
                                
                                --Mediante una lista por compresión obtenemos los valores de las distancias Manhattan segun el cromosoma values in range introducido y 
                                --concatenamos la distancia entre el primer punto y el último (el cromosoma sólo indica el orden en el que evaluamos las distancias)
                                -- (Funcion de minimización)
                                -- Posible cromosoma:  [0..19]

--input: Dos puntos en dos dimensiones
--output: La distancia entre dichos puntos
distanciaManhattan :: (Int,Int) -> (Int,Int) -> Double
distanciaManhattan (a1,a2) (b1,b2) = sqrt ( fromIntegral(((a1-b1)^2)) + fromIntegral(((a2-b2)^2)) )


----------------------------------------------------------------------------------------------------------------------------------

--FITNESS CUADRADO MÁGICO

--input: un cromosoma de números enteros
--output: valoración de dicho cromosoma obtenida mediante la suma en valor absoluto de las diferencias de cada fila, columna y diagonal y 15
fitnessCuadradoMagico :: [Int] -> Double
fitnessCuadradoMagico xs = fitnessCuadradoMagicoAux (M.fromList 3 3 xs)
    
fitnessCuadradoMagicoAux :: Matrix Int -> Double
fitnessCuadradoMagicoAux matriz = fromIntegral (Prelude.sum (Prelude.map (abs) (Prelude.map (resta15) listaSumas))) where
    listaSumas = sumaDiagonales Prelude.++ sumaColumnas Prelude.++ sumaFilas where
        sumaDiagonales = [(Prelude.sum [getElem x x matriz| x <-[1..(Prelude.length (toLists matriz))]]),
            (Prelude.sum [getElem x y matriz| x <-[1..(Prelude.length (toLists matriz))] , y<-[1..(Prelude.length (toLists matriz))] , x+y-1 == Prelude.length (toLists matriz)])] 
        sumaColumnas = [Prelude.sum (V.toList (getCol x matriz)) | x <- [1..(Prelude.length ((toLists matriz)!!0))]]
        sumaFilas = [Prelude.sum (V.toList (getRow x matriz)) | x <- [1..(Prelude.length (toLists matriz))]]
                                -- Lista ejemplo de cuadrado mágico [4,3,8,9,5,1,2,7,6] 
                                -- En esta función restamos 15, que es la suma que obtenemos en un cuadrado mágico 3x3, al resultado de cada una de las sumas obtenidas de filas,
                                -- columnas y diagonales las cuales se obtienen mediante listas por compresión y extracciones de matrices

resta15 :: Int -> Int
resta15 x = x-15    
    

-------------------------------------------------------------------------------------------------------------------------------

--FITNESS SUDOKU

--input: un cromosoma de números enteros 
--output: la valoración de dicho cromosoma obtenida mediante la suma acumulada del número de veces que se repite un número en una fila, columna o submatriz
fitnessSudoku :: [Int] -> Double
fitnessSudoku xs = fitnessSudokuAux (M.fromList 9 9 xs)

fitnessSudokuAux :: Matrix Int -> Double --Un sudoku será valido cuando la función fitness devuelva 0, minimizacion
fitnessSudokuAux matriz = (Prelude.sum listaSumas)  where
    listaSumas = sumaColumnas Prelude.++ sumaFilas Prelude.++ sumaCuadrados where
        sumaColumnas = [cuentaIguales (V.toList (getCol x matriz)) | x <- [1..9]]
        sumaFilas = [cuentaIguales (V.toList (getRow x matriz)) | x <- [1..9]]
        sumaCuadrados = [cuentaIguales (M.toList (submatrix x y x y matriz)) | x <- [1,4,7], y <- [3,6,9], x<y, y-x==2]
--Ejemplo sudoku [2,1,9,5,4,3,6,7,8,5,4,3,8,7,6,9,1,2,8,7,6,2,1,9,3,4,5,4,3,2,7,6,5,8,9,1,7,6,5,1,9,8,2,3,4,1,9,8,4,3,2,5,6,7,3,2,1,6,5,4,7,8,9,6,5,4,9,8,7,1,2,3,9,8,7,3,2,1,4,5,6]
--posible cromosoma: Prelude.concat [[1..9] | x<-[0..8]]


--input: una lista de enteros
--output: apariciones adicionales de cada número entero en la lista módulo 9
cuentaIguales :: [Int] -> Double 
cuentaIguales xs = igualesAux xs 0.0

igualesAux :: [Int] -> Double -> Double
igualesAux [] acum = acum 
igualesAux (x:xs) acum = igualesAux (borra x xs) (acum + (fromIntegral (Prelude.length (Prelude.filter (congruente x 9) (xs))))) 

--input: 3 numeros enteros 
--output: devuelve True si el primer número y el tercero son congruentes siendo el módulo el segundo, en caso contrario devuelve False
congruente :: Int -> Int -> Int -> Bool
congruente num1 modulo num2
    | (num1 `mod` modulo) == (num2 `mod` modulo) = True
    | otherwise = False

--input: un numero entero y una lista de números enteros
--output: la lista de entrada, eliminando todos los valores iguales al número entero introducido
borra :: Int -> [Int] -> [Int]
borra k xs = Prelude.foldr (\x ac -> if x==k then ac else x:ac) [] xs

--Reformulacion de la operación mod para poder aplicarla con map
mod2 :: Int -> Int -> Int
mod2 modulo num = num `mod` modulo 

------------------------------------------------------------------------------------------------------------------------------
--FITNESS MOCHILA

--Input: Lista generada del tipo [e1, ..., en] donde cada ei representa el número veces que tenemos el objeto ei
--       Lista ts: Indica la cantidad de cada objeto ei que tenemos disponible
--       Lista bs: Indica el valor de cada objeto ei
--       Lista ws: Indica el peso de cada objeto ei
--       W: Indica el peso máximo que soporta nuestra mochila
fitnessMochila :: [Int] -> Double
fitnessMochila xs = fitnessMochilaAux xs [1,2,3,2,1,2,3,1,2,1] [5,3,2,6,4,2,4,2,4,1] [4,3,1,5,6,2,7,8,3,2] 20

fitnessMochilaAux :: [Int] -> [Int] -> [Int] -> [Int] -> Int -> Double
fitnessMochilaAux xs ts bs ws w
    | difPesos < 0  = f - penalty
    | otherwise     = f
    where f = fromIntegral (Prelude.sum (multiplicaListas xs bs)) --suma de los beneficios de cada objeto
          penalty = fromIntegral ((Prelude.sum ws) * abs(dotMultXsWs - w))
          difPesos = fromIntegral(w - dotMultXsWs)
          dotMultXsWs = Prelude.sum(multiplicaListas xs ws)
          --penaliza teniendo en cuenta la diferencia entre el peso objetivo y el de la lista parámetro


--Devuelve el número de elementos de dos listas que cumplen la condición x<b
checkNumElems :: [Int] -> [Int] -> Int
checkNumElems xs bs = Prelude.length (Prelude.filter (==True) (Prelude.zipWith(<) xs bs))

multiplicaListas :: [Int] -> [Int] -> [Int]
multiplicaListas = Prelude.zipWith (*)


--------------------------------------------------------------------------------
--DECODIFICADORES DE LOS DIFERENTES EJEMPLOS

--input: un cromosoma de números enteros
--output: imprime por pantalla la matriz que representa dicho cromosoma
decodificaCuadradoMagico :: [Int] -> IO ()
decodificaCuadradoMagico xs = print (M.fromList 3 3 xs)

--input: un cromosoma de números enteros
--output: imprime por pantalla la matriz que representa dicho cromosoma 
decodificaSudoku :: [Int] -> IO ()
decodificaSudoku xs = print (M.fromList 9 9 (Prelude.map (+1) (Prelude.map (mod2 9) xs)))

--input: un cromosoma de números enteros
--output: imprime por pantalla la ordenación de puntos de problema del viajante representada por dicho cromosoma
decodificaViajante :: [Int] -> IO ()
decodificaViajante xs = print [(valoresViajante 10)!!x | x<-xs] 

--input: un cromosoma de números enteros
--output: imprime por pantalla dicho cromosoma
decodificaMochila :: [Int] -> IO ()
decodificaMochila xs = print xs


-------------------------------------------------------------------------------------------------
--CRITERIOS DE PARADA


--input: una funcion fitness y una lista de cromosomas
--output: devuelve el mejor cromosoma de la lista (maximizando)
--sirve para los criterios de parada
mejorMax :: ([Int]->Double) -> [[Int]] -> [Int]
mejorMax fitness xs = (seleccionElitistaMinimizar xs 1 fitness)!!0

--input: una funcion fitness y una lista de cromosomas
--output: devuelve el mejor cromosoma de la lista (minimizando)
--sirve para los criterios de parada
mejorMin :: ([Int]->Double) -> [[Int]] -> [Int]
mejorMin fitness xs = (seleccionElitistaMinimizar xs 1 fitness)!!0