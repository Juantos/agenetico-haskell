module GeneticoEx(
    numIteraciones,
    poblacion,
    tCromosoma,
    tamanoCromosoma,
    valoresGenRange,
    valoresGenPermutation,
    porcentajeMezcla
) where

import AGenetico
import Fitness
import Generador
import Data.Matrix
import Data.List

--PARÁMETROS DEL ALGORITMO GENÉTICO
numIteraciones :: Int --criterio de parada del algoritmo genético 
numIteraciones = 10

valorObjetivo :: Double --criterio de parada del algoritmo genético
valorObjetivo = 20.0

poblacion :: Int --número de cromosomas que se evalúan en cada iteración (múltiplo de 100)
poblacion = 400

data TipoCromosoma = ValuesInRange | Permutation  --valores posibles "ValuesInRange" "Permutation"
                                        deriving Eq 
tCromosoma :: TipoCromosoma
tCromosoma = Permutation
                        

tamanoCromosoma :: Int --length de la lista que representa al cromosoma
tamanoCromosoma = 20  -- Cuadrado Magico 9 -- Viajante 20 -- Sudoku 81

valoresGenRange :: (Int,Int) --valor mínimo y máximo que puede tomar un gen en el cromosoma (para ValuesInRange)
valoresGenRange = (1,9) -- Sudoku (1,9)

valoresGenPermutation :: [Int]
valoresGenPermutation = [0..19] -- Cuadrado Magico [1..9] -- Viajante [0..19] -- 

porcentajeMezcla :: [(String,Int)] --Describe que mutaciones y combinaciones utilizar y en qué porcentaje
porcentajeMezcla = [("padres",(div (20*poblacion) 100))
                    ,("comb1",(div (0*poblacion) 100))
                    ,("comb2",(div(0*poblacion) 100))
                    ,("combCiclos",(div(60*poblacion) 100))
                    ,("mut1",(div(0*poblacion) 100))
                    ,("mutInter",(div(10*poblacion) 100))
                    ,("random",(div(10*poblacion) 100))]


data Objetivo = Max | Min  --Objetivo de la funcion
obj :: Objetivo
obj = Min

data MetodoSeleccion = Elitista   --Metodo de selección utilizado por el algoritmo
                                        deriving Eq  
mSeleccion :: MetodoSeleccion
mSeleccion = Elitista

fitness :: ([Int] -> Double)
fitness = fitnessViajante -- Cuadrado Magico fitnessCuadradoMagico -- Viajante fitnessViajante -- Sudoku fitnessSudoku

decodifica :: ([Int] -> IO())
decodifica = decodificaViajante -- Cuadrado Magico decodificaCuadradoMagico -- Viajante decodificaViajante -- Sudoku decodificaSudoku


--Cuando trabajamos con cromosomas de tipo Permutation, solo podemos utilizar combinaciones y mutaciones que alteren las posiciones de los genes sin que el conjunto de valores cambie
--La suma de los porcentajes debe ser 100


itera :: IO [[Int]] -> TipoCromosoma -> Objetivo -> IO [[Int]]
itera xs Permutation Min = do
    lista <- xs
    let padres = seleccionElitistaMinimizar lista ((snd (porcentajeMezcla!!0))) fitness 
    let random = seleccionElitistaMaximizar lista (div ((100-(div ((snd (porcentajeMezcla!!0))*100) poblacion))*poblacion) 100) fitness -- hace la lista de los no seleccionados
    comb1 <- ejecutaCombinacion1 padres tamanoCromosoma porcentajeMezcla
    comb2 <- ejecutaCombinacion2 padres porcentajeMezcla
    combCiclos <- ejecutaCombinacionCiclos padres tamanoCromosoma porcentajeMezcla
    mut1 <- ejecutaMutacion1Int padres tamanoCromosoma valoresGenRange porcentajeMezcla
    permutInter <- ejecutaPermutacionInter padres tamanoCromosoma porcentajeMezcla
    random <- seleccionAleatoria random ((snd (porcentajeMezcla!!6))) 
    return (padres++comb1++comb2++combCiclos++mut1++permutInter++random)


ejecutaGenetico :: IO () --Imprime: el cromosoma seleccionado, el cromosoma decodificado y el valor de su fitness
ejecutaGenetico = do
    if (tCromosoma == Permutation) 
        then do
            cr <- iteraciones (generaPoblacionPermutation valoresGenPermutation poblacion) tCromosoma obj numIteraciones 
            print cr
            decodifica cr
            print (fitness cr)
        else do
            cr <- iteraciones (generaPoblacion tamanoCromosoma poblacion (fst valoresGenRange) (snd valoresGenRange)) tCromosoma obj numIteraciones 
            print cr
            decodifica cr
            print (fitness cr)






iteraciones ::  IO[[Int]] -> TipoCromosoma -> Objetivo -> Int -> IO[Int]
iteraciones xs _ Max 0 = do
    lista <- xs
    return (mejorMax fitness lista)
iteraciones xs _ Min 0 = do
    lista <- xs
    return (mejorMin fitness lista)
iteraciones xs tcromosoma Max it = do
    lista <- xs
    if (fitness (mejorMax fitness lista)==valorObjetivo)
        then do
            return (mejorMax fitness lista)
        else do 
            r <- (iteraciones (itera xs tcromosoma Max) tcromosoma Max (it-1))
            return r
iteraciones xs tcromosoma Min it = do
    lista <- xs
    if (fitness (mejorMin fitness lista)==valorObjetivo)
        then do
            return (mejorMin fitness lista)
        else do 
            r <- (iteraciones (itera xs tcromosoma Min) tcromosoma Min (it-1))
            return r





--iteraciones xs tcromosoma Min it =

--    | valorObjetivo == fitness (mejorMax fitness xs) = mejorMax fitness xs
--    | it == 0 = (mejorMax fitness xs)
--    | otherwise = iteraciones (itera xs tcromosoma Max) tcromosoma Max (it-1)
--iteraciones xs tcromosoma Min it 
--    | valorObjetivo == fitness fst ((sortBy ordena (zip xs (map fitness xs))) !! 0) = 
--    | it == 0 = fitness fst ((sortBy ordena (zip xs (map fitness xs))) !! 0) 
--    | otherwise = iteraciones (itera xs tcromosoma Min) tcromosoma Min (it-1)
        


--prueba: itera (generaPoblacionPermutation [1..9] 100) Permutation Min

--pruebaElitista :: IO [[Int]]
--pruebaElitista = do
--   let r = seleccionElitistaMinimizar ([[3,5,6,7,8,1,4,9,2],[1,8,2,6,9,4,7,3,5],[7,2,6,1,3,8,9,4,5],[6,4,3,2,8,5,9,7,1],[7,3,4,9,6,5,1,8,2],[4,8,1,6,3,7,9,5,2],[6,9,2,3,8,4,7,5,1],[1,2,9,5,6,4,7,8,3],[1,8,7,4,2,5,9,6,3]]) 2 fitnessCuadradoMagico
--   return r
