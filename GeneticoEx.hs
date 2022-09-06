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
numIteraciones = 20


valorObjetivo :: Double --criterio de parada del algoritmo genético
valorObjetivo = 0.0

poblacion :: Int --número de cromosomas que se evalúan en cada iteración (múltiplo de 100)
poblacion = 400

data TipoCromosoma = ValuesInRange | Permutation  --valores posibles "ValuesInRange" "Permutation"
                                        deriving Eq 
tCromosoma :: TipoCromosoma
tCromosoma = ValuesInRange -- Permutation para cuadrado, viajante y sudoku
                        --ValuesInRange para mochila
                        

tamanoCromosoma :: Int --length de la lista que representa al cromosoma
tamanoCromosoma = 10  -- Cuadrado Magico 9 -- Viajante 20 -- Sudoku 81 -- Mochila 10

valoresGenRange :: (Int,Int) --valor mínimo y máximo que puede tomar un gen en el cromosoma (para ValuesInRange)
valoresGenRange = (0,50) -- Mochila (0,50)

valoresGenPermutation :: [Int]
valoresGenPermutation = [0..19] -- Cuadrado Magico [1..9] -- Viajante [0..19] -- Sudoku [1..81]

porcentajeMezcla :: [(String,Int)] --Describe que mutaciones y combinaciones utilizar y en qué porcentaje
porcentajeMezcla = [("padres",(div (20*poblacion) 100))
                    ,("comb1",(div (20*poblacion) 100))
                    ,("comb2",(div(30*poblacion) 100))
                    ,("combCiclos",(div(0*poblacion) 100))
                    ,("mut1",(div(10*poblacion) 100))
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
fitness = fitnessMochila -- Cuadrado Magico fitnessCuadradoMagico -- Viajante fitnessViajante -- Sudoku fitnessSudoku

decodifica :: ([Int] -> IO())
decodifica = decodificaViajante -- Cuadrado Magico decodificaCuadradoMagico -- Viajante decodificaViajante -- Sudoku decodificaSudoku


--Cuando trabajamos con cromosomas de tipo Permutation, solo podemos utilizar combinaciones y mutaciones que alteren las posiciones de los genes sin que el conjunto de valores cambie
--La suma de los porcentajes debe ser 100


itera :: IO [[Int]] -> TipoCromosoma -> Objetivo -> IO [[Int]]
itera xs tcromosoma Min = do
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
itera xs tcromosoma Max = do
    lista <- xs
    let padres = seleccionElitistaMaximizar lista ((snd (porcentajeMezcla!!0))) fitness 
    let random = seleccionElitistaMinimizar lista (div ((100-(div ((snd (porcentajeMezcla!!0))*100) poblacion))*poblacion) 100) fitness -- hace la lista de los no seleccionados
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
            cr <- iteraciones (generaPoblacion (fst valoresGenRange) (snd valoresGenRange) tamanoCromosoma poblacion ) tCromosoma obj numIteraciones 
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




