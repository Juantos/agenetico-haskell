module MainGenetico where

import Fitness
import Generador
import GeneticoEx

import System.Random
import Control.Monad (replicateM)


ejecutaMochila :: IO()
ejecutaMochila = do
    ejecutaGenetico fitnessMochila 
                    tipoCromosomaMochila
                    tamanoCromosomaMochila 
                    valorObjetivoMochila 
                    valoresGenRangeMochila 
                    [] 
                    porcentajeMezclaMochila 
                    poblacionMochila 
                    numIteracionesMochila 
                    objMochila
                    decodificaMochila
    
ejecutaViajante :: IO()
ejecutaViajante = do
    ejecutaGenetico fitnessViajante 
                    tipoCromosomaViajante 
                    tamanoCromosomaViajante 
                    valorObjetivoViajante 
                    (0,0)
                    valoresGenPermutationViajante 
                    porcentajeMezclaViajante 
                    poblacionViajante 
                    numIteracionesViajante 
                    objViajante
                    decodificaViajante

ejecutaCuadradoMagico :: IO()
ejecutaCuadradoMagico = do
    ejecutaGenetico fitnessCuadradoMagico 
                    tipoCromosomaCuadradoMagico 
                    tamanoCromosomaCuadradoMagico 
                    valorObjetivoCuadradoMagico 
                    (0,0)
                    valoresGenPermutationCuadradoMagico 
                    porcentajeMezclaCuadradoMagico 
                    poblacionCuadradoMagico 
                    numIteracionesCuadradoMagico 
                    objCuadradoMagico
                    decodificaCuadradoMagico

ejecutaSudoku :: IO()
ejecutaSudoku = do
    ejecutaGenetico fitnessSudoku 
                    tipoCromosomaSudoku 
                    tamanoCromosomaSudoku 
                    valorObjetivoSudoku 
                    (0,0)
                    valoresGenPermutationSudoku 
                    porcentajeMezclaSudoku 
                    poblacionSudoku 
                    numIteracionesSudoku 
                    objSudoku
                    decodificaSudoku
    
ejecutaGenetico :: ([Int] -> Double) -> TipoCromosoma -> Int -> Double -> (Int, Int) -> [Int] -> [(String,Int)] -> Int -> Int -> Objetivo -> ([Int] -> IO ()) -> IO () --Imprime: el cromosoma seleccionado, el cromosoma decodificado y el valor de su fitness
ejecutaGenetico fitness tipoCromosoma tamanoCromosoma valorObjetivo valoresGenRange valoresGenPermutation porcentajeMezcla poblacion numIteraciones obj decodifica = do
    if (tipoCromosoma == Permutation) 
        then do
            cr <- iteraciones (generaPoblacionPermutation valoresGenPermutation poblacion) 
                                tipoCromosoma 
                                obj 
                                numIteraciones 
                                valorObjetivo 
                                valoresGenRange 
                                tamanoCromosoma 
                                poblacion 
                                porcentajeMezcla 
                                fitness
            print cr
            decodifica cr
            print (fitness cr)
        else do
            cr <- iteraciones (generaPoblacion (fst valoresGenRange) (snd valoresGenRange) tamanoCromosoma poblacion ) 
                                tipoCromosoma
                                obj 
                                numIteraciones 
                                valorObjetivo 
                                valoresGenRange 
                                tamanoCromosoma 
                                poblacion 
                                porcentajeMezcla 
                                fitness
            print cr
            decodifica cr
            print (fitness cr)


--PARÁMETROS MOCHILA
numIteracionesMochila :: Int --criterio de parada del algoritmo genético 
numIteracionesMochila = 400 --100, 200, 400 mochila

objMochila :: Objetivo
objMochila = Max

valorObjetivoMochila :: Double --criterio de parada del algoritmo genético
valorObjetivoMochila = 2000

poblacionMochila :: Int --número de cromosomas que se evalúan en cada iteración
poblacionMochila = 20 --30, 20, 20 mochila

tipoCromosomaMochila :: TipoCromosoma
tipoCromosomaMochila = ValuesInRange -- Permutation para cuadrado, viajante y sudoku
                        --ValuesInRange para mochila             

tamanoCromosomaMochila :: Int --length de la lista que representa al cromosoma
tamanoCromosomaMochila = 10  -- Cuadrado Magico 9 -- Viajante 20 -- Sudoku 81 -- Mochila 10

valoresGenRangeMochila :: (Int,Int) --valor mínimo y máximo que puede tomar un gen en el cromosoma (para ValuesInRange)
valoresGenRangeMochila = (0,3) -- Mochila (0,objetoenmayorcantidad)

porcentajeMezclaMochila :: [(String,Int)] --Describe que mutaciones y combinaciones utilizar y en qué porcentaje
porcentajeMezclaMochila = [("padres",(div (20*poblacionMochila) 100))
                    ,("comb1",(div (20*poblacionMochila) 100))
                    ,("comb2",(div(30*poblacionMochila) 100))
                    ,("combCiclos",(div(0*poblacionMochila) 100))
                    ,("mut1",(div(20*poblacionMochila) 100))
                    ,("mutInter",(div(0*poblacionMochila) 100))
                    ,("random",(div(10*poblacionMochila) 100))]


--PARÁMETROS VIAJANTE
numIteracionesViajante :: Int
numIteracionesViajante = 30

objViajante :: Objetivo
objViajante = Min

valorObjetivoViajante :: Double --criterio de parada del algoritmo genético
valorObjetivoViajante = 20.0 

poblacionViajante :: Int --número de cromosomas que se evalúan en cada iteración
poblacionViajante = 100

tipoCromosomaViajante :: TipoCromosoma
tipoCromosomaViajante = Permutation -- Permutation para cuadrado, viajante y sudoku         

tamanoCromosomaViajante :: Int --length de la lista que representa al cromosoma
tamanoCromosomaViajante = 20 

valoresGenPermutationViajante :: [Int]
valoresGenPermutationViajante = [0..19] -- Cuadrado Magico [1..9] -- Viajante [0..19] -- Sudoku [1..81]

porcentajeMezclaViajante :: [(String,Int)] --Describe que mutaciones y combinaciones utilizar y en qué porcentaje
porcentajeMezclaViajante = [("padres",(div (20*poblacionViajante) 100))
                    ,("comb1",(div (0*poblacionViajante) 100))
                    ,("comb2",(div(0*poblacionViajante) 100))
                    ,("combCiclos",(div(60*poblacionViajante) 100))
                    ,("mut1",(div(0*poblacionViajante) 100))
                    ,("mutInter",(div(10*poblacionViajante) 100))
                    ,("random",(div(10*poblacionViajante) 100))]

--PARÁMETROS CUADRADO MÁGICO
numIteracionesCuadradoMagico :: Int --criterio de parada del algoritmo genético 
numIteracionesCuadradoMagico = 200

objCuadradoMagico :: Objetivo
objCuadradoMagico = Min

valorObjetivoCuadradoMagico :: Double --criterio de parada del algoritmo genético
valorObjetivoCuadradoMagico = 0.0 

poblacionCuadradoMagico :: Int --número de cromosomas que se evalúan en cada iteración
poblacionCuadradoMagico = 20

tipoCromosomaCuadradoMagico :: TipoCromosoma
tipoCromosomaCuadradoMagico = Permutation

tamanoCromosomaCuadradoMagico :: Int --length de la lista que representa al cromosoma
tamanoCromosomaCuadradoMagico = 9 

valoresGenPermutationCuadradoMagico :: [Int]
valoresGenPermutationCuadradoMagico = [1..9]

porcentajeMezclaCuadradoMagico :: [(String,Int)] --Describe que mutaciones y combinaciones utilizar y en qué porcentaje
porcentajeMezclaCuadradoMagico = [("padres",(div (10*poblacionCuadradoMagico) 100))
                    ,("comb1",(div (0*poblacionCuadradoMagico) 100))
                    ,("comb2",(div(0*poblacionCuadradoMagico) 100))
                    ,("combCiclos",(div(50*poblacionCuadradoMagico) 100))
                    ,("mut1",(div(0*poblacionCuadradoMagico) 100))
                    ,("mutInter",(div(30*poblacionCuadradoMagico) 100))
                    ,("random",(div(10*poblacionCuadradoMagico) 100))]


--PARÁMETROS SUDOKU
numIteracionesSudoku :: Int --criterio de parada del algoritmo genético 
numIteracionesSudoku = 100

objSudoku :: Objetivo
objSudoku = Min

valorObjetivoSudoku :: Double 
valorObjetivoSudoku = 0.0 

poblacionSudoku :: Int --número de cromosomas que se evalúan en cada iteración
poblacionSudoku = 40 --30, 20, 20 Sudoku

tipoCromosomaSudoku :: TipoCromosoma
tipoCromosomaSudoku = Permutation  

tamanoCromosomaSudoku :: Int --length de la lista que representa al cromosoma
tamanoCromosomaSudoku = 81 

valoresGenPermutationSudoku :: [Int]
valoresGenPermutationSudoku = [1..81]

porcentajeMezclaSudoku :: [(String,Int)] --Describe que mutaciones y combinaciones utilizar y en qué porcentaje
porcentajeMezclaSudoku = [("padres",(div (10*poblacionSudoku) 100))
                    ,("comb1",(div (0*poblacionSudoku) 100))
                    ,("comb2",(div(0*poblacionSudoku) 100))
                    ,("combCiclos",(div(50*poblacionSudoku) 100))
                    ,("mut1",(div(0*poblacionSudoku) 100))
                    ,("mutInter",(div(30*poblacionSudoku) 100))
                    ,("random",(div(10*poblacionSudoku) 100))]
