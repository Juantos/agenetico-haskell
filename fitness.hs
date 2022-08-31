
import AGenetico
import Data.Matrix as M
import Data.Vector as V



--FUNCIONES FITNESS

valoresViajante :: Int -> [(Int,Int)] --Coordenadas que utilizaremos para el problema del viajante (para asegurar que el camino mínimo sea 4n)
valoresViajante n = [(x,y) | x <- [0..n] , y <- [0..n] , (x==0 || y==0 || x==n || y==n)]

fitnessProblemaViajante :: [Int] -> Double 
fitnessProblemaViajante xs =  Prelude.sum ([distanciaManhattan ((valoresViajante 10)!!(xs!!x)) ((valoresViajante 10)!!(xs!!(x+1))) | x <- [0..((Prelude.length xs)-2)]]  
                                Prelude.++  [distanciaManhattan ((valoresViajante 10) !! 0) ((valoresViajante 10) !! 39)])
                                
                                --Mediante una lista por compresión obtenemos los valores de las distancias Manhattan segun el cromosoma values in range introducido y 
                                --concatenamos la distancia entre el primer punto y el último (el cromosoma sólo indica el orden en el que evaluamos las distancias)
                                -- (Funcion de minimización)

distanciaManhattan :: (Int,Int) -> (Int,Int) -> Double
distanciaManhattan (a1,a2) (b1,b2) = sqrt ( fromIntegral(((a1-b1)^2)) + fromIntegral(((a2-b2)^2)) )

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
    

