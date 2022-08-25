
import AGenetico



--FUNCIONES FITNESS
--valoresMochila :: [(Int,Int)] --Lista de pesos y valores para la función fitness del problema de la mochila
--valoresMochila = [(20,40),(10,18),(5,7),(7,8),(17,38)]

--fitnessMochila :: [Int] -> [(Int,Int)] -> Int -> Int --Función fitness del problema de la mochila, en el cual se optimiza los objetos a portar en una mochila con dado un peso límite y los pesos y valores de los objetos
--fitnessMochila xs vs p 

valoresViajante :: Int -> [(Int,Int)] --Coordenadas que utilizaremos para el problema del viajante (para asegurar que el camino mínimo sea 4n)
valoresViajante n = [(x,y) | x <- [0..n] , y <- [0..n] , (x==0 || y==0 || x==n || y==n)]

fitnessProblemaViajante :: [Int] -> Double
fitnessProblemaViajante xs =  sum ([distanciaManhattan ((valoresViajante 10)!!(xs!!x)) ((valoresViajante 10)!!(xs!!(x+1))) | x <- [0..((length xs)-2)]]  
                                ++  [distanciaManhattan ((valoresViajante 10) !! 0) ((valoresViajante 10) !! 39)])

distanciaManhattan :: (Int,Int) -> (Int,Int) -> Double
distanciaManhattan (a1,a2) (b1,b2) = sqrt ( fromIntegral(((a1-b1)^2)) + fromIntegral(((a2-b2)^2)) )


