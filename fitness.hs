


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

) where

import AGenetico
import Data.Matrix as M
import Data.Vector as V




--FUNCIONES FITNESS

valoresViajante :: Int -> [(Int,Int)] --Coordenadas que utilizaremos para el problema del viajante (para asegurar que el camino mínimo sea 4n)
valoresViajante n = [(x,y) | x <- [0..n] , y <- [0..n] , (x==0 || y==0 || x==n || y==n)]

fitnessViajante :: [Int] -> Double 
fitnessViajante xs =  Prelude.sum ([distanciaManhattan ((valoresViajante 5)!!(xs!!x)) ((valoresViajante 5)!!(xs!!(x+1))) | x <- [0..((Prelude.length xs)-2)]]  
                        Prelude.++  [distanciaManhattan ((valoresViajante 5) !! 0) ((valoresViajante 5) !! 19)])
                                
                                --Mediante una lista por compresión obtenemos los valores de las distancias Manhattan segun el cromosoma values in range introducido y 
                                --concatenamos la distancia entre el primer punto y el último (el cromosoma sólo indica el orden en el que evaluamos las distancias)
                                -- (Funcion de minimización)
                                -- Posible cromosoma:  [0..19]

distanciaManhattan :: (Int,Int) -> (Int,Int) -> Double
distanciaManhattan (a1,a2) (b1,b2) = sqrt ( fromIntegral(((a1-b1)^2)) + fromIntegral(((a2-b2)^2)) )

                                --posible cromosoma: [1..9]
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
    
--Ejemplo sudoku [2,1,9,5,4,3,6,7,8,5,4,3,8,7,6,9,1,2,8,7,6,2,1,9,3,4,5,4,3,2,7,6,5,8,9,1,7,6,5,1,9,8,2,3,4,1,9,8,4,3,2,5,6,7,3,2,1,6,5,4,7,8,9,6,5,4,9,8,7,1,2,3,9,8,7,3,2,1,4,5,6]
--posible cromosoma: Prelude.concat [[1..9] | x<-[0..8]]
fitnessSudoku :: [Int] -> Double
fitnessSudoku xs = fitnessSudokuAux (M.fromList 9 9 xs)

fitnessSudokuAux :: Matrix Int -> Double --Un sudoku será valido cuando la función fitness devuelva 0, minimizacion
fitnessSudokuAux matriz = (Prelude.sum listaSumas) + (100*cuentaIguales (M.toList matriz)) where
    listaSumas = sumaColumnas Prelude.++ sumaFilas Prelude.++ sumaCuadrados where
        sumaColumnas = [cuentaIguales (V.toList (getCol x matriz)) | x <- [1..9]]
        sumaFilas = [cuentaIguales (V.toList (getRow x matriz)) | x <- [1..9]]
        sumaCuadrados = [cuentaIguales (M.toList (submatrix x y x y matriz)) | x <- [1,4,7], y <- [3,6,9], x<y, y-x==2]

cuentaIguales :: [Int] -> Double 
cuentaIguales xs = igualesAux xs 0.0

igualesAux :: [Int] -> Double -> Double
igualesAux [] acum = acum 
igualesAux (x:xs) acum = igualesAux (borra x xs) (acum + (fromIntegral (Prelude.length (Prelude.filter (==x) (xs))))) 

borra :: Int -> [Int] -> [Int]
borra k xs = Prelude.foldr (\x ac -> if x==k then ac else x:ac) [] xs

decodificaCuadradoMagico :: [Int] -> IO ()
decodificaCuadradoMagico xs = print (M.fromList 3 3 xs)

decodificaSudoku :: [Int] -> IO ()
decodificaSudoku xs = print (M.fromList 9 9 xs)

decodificaViajante :: [Int] -> IO ()
decodificaViajante xs = print [(valoresViajante 10)!!x | x<-xs] 

mejorMax :: ([Int]->Double) -> [[Int]] -> [Int]
mejorMax fitness xs = (seleccionElitistaMinimizar xs 1 fitness)!!0

mejorMin :: ([Int]->Double) -> [[Int]] -> [Int]
mejorMin fitness xs = (seleccionElitistaMinimizar xs 1 fitness)!!0