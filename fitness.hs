
import AGenetico



--FUNCIONES FITNESS
--valoresMochila :: [(Int,Int)] --Lista de pesos y valores para la función fitness del problema de la mochila
--valoresMochila = [(20,40),(10,18),(5,7),(7,8),(17,38)]

--fitnessMochila :: [Int] -> [(Int,Int)] -> Int -> Int --Función fitness del problema de la mochila, en el cual se optimiza los objetos a portar en una mochila con dado un peso límite y los pesos y valores de los objetos
--fitnessMochila xs vs p 

valoresViajante :: Int -> [(Int,Int)] --Coordenadas que utilizaremos para el problema del viajante (para asegurar que el camino mínimo sea 4n)
valoresViajante n = [(x,y) | x <- [0..n-1] , y <- [0..n-1]]

fitnessProblemaViajante :: [] -> Double
fitnessProblemaViajante 