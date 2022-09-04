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


--PARÁMETROS DEL ALGORITMO GENÉTICO
numIteraciones :: Int --criterio de parada del algoritmo genético 
numIteraciones = 10000

poblacion :: Int --número de cromosomas que se evalúan en cada iteración (múltiplo de 100)
poblacion = 100

data TipoCromosoma = ValuesInRange | Permutation --valores posibles "ValuesInRange" "Permutation"
tCromosoma :: TipoCromosoma
tCromosoma = Permutation

tamanoCromosoma :: Int --length de la lista que representa al cromosoma
tamanoCromosoma = 9  --valores para el ejemplo del cuadrado mágico

valoresGenRange :: (Int,Int) --valor mínimo y máximo que puede tomar un gen en el cromosoma (para ValuesInRange)
valoresGenRange = (0,100)

valoresGenPermutation :: [Int]
valoresGenPermutation = [1..9] --valores para el ejemplo del cuadrado mágico

porcentajeMezcla :: [(String,Int)] --Describe que mutaciones y combinaciones utilizar y en qué porcentaje
porcentajeMezcla = [("padres",20),("comb1",10),("comb2",1),("combCiclos",70),("mut1",2),("mutInter",5),("mutInser",5)]

data Objetivo = Max | Min  --Objetivo de la funcion
obj :: Objetivo
obj = Min

data MetodoSeleccion = Elitista | Ruleta  --Metodo de selección utilizado por el algoritmo
                                        deriving Eq  
mSeleccion :: MetodoSeleccion
mSeleccion = Elitista

--Cuando trabajamos con cromosomas de tipo Permutation, solo podemos utilizar combinaciones y mutaciones que alteren las posiciones de los genes sin que el conjunto de valores cambie
--La suma de los porcentajes debe ser 100


itera :: IO [[Int]] -> TipoCromosoma -> Objetivo -> IO [[Int]]
itera xs Permutation Min = do
    lista <- xs
    let padres = {-if (mSeleccion == Elitista) then -} seleccionElitistaMaximizar lista ((snd (porcentajeMezcla!!0))) fitnessCuadradoMagico 
        --else seleccionRuleta xs ((snd (porcentajeMezcla!!0))*(poblacion/100)) fitnessCuadradoMagico 
    comb1 <- ejecutaCombinacion1 padres tamanoCromosoma porcentajeMezcla
    comb2 <- ejecutaCombinacion2 padres porcentajeMezcla
    combCiclos <- ejecutaCombinacionCiclos padres tamanoCromosoma porcentajeMezcla
    mut1 <- ejecutaMutacion1Int padres tamanoCromosoma valoresGenRange porcentajeMezcla
    permutInter <- ejecutaPermutacionInter padres tamanoCromosoma porcentajeMezcla
    permutInser <- ejecutaPermutacionInser padres tamanoCromosoma porcentajeMezcla
    --return (padres++comb1++comb2++combCiclos++mut1++permutInter++permutInser)
    return (padres ++ [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]] ++ permutInser)

--PROBLEMA ACTUAL: necesitamos declarar una barbaridad de numeros aleatorios
 




--prueba: itera (generaPoblacionPermutation [1..9] 100) Permutation Min



--pruebaElitista :: IO [[Int]]
--pruebaElitista = do
--   let r = seleccionElitistaMinimizar ([[3,5,6,7,8,1,4,9,2],[1,8,2,6,9,4,7,3,5],[7,2,6,1,3,8,9,4,5],[6,4,3,2,8,5,9,7,1],[7,3,4,9,6,5,1,8,2],[4,8,1,6,3,7,9,5,2],[6,9,2,3,8,4,7,5,1],[1,2,9,5,6,4,7,8,3],[1,8,7,4,2,5,9,6,3]]) 2 fitnessCuadradoMagico
--   return r
