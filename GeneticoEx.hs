module GeneticoEx(
    decodifica,
    iteraciones,
    TipoCromosoma(ValuesInRange, Permutation),
    Objetivo(Min, Max)
    
) where

import VariacionesCMS
import Fitness

import Data.Matrix
import Data.List

data TipoCromosoma = ValuesInRange | Permutation  --valores posibles "ValuesInRange" "Permutation"
                                        deriving Eq 


data Objetivo = Max | Min  --Objetivo de la funcion

data MetodoSeleccion = Elitista   --Metodo de selección utilizado por el algoritmo
                                        deriving Eq  
mSeleccion :: MetodoSeleccion
mSeleccion = Elitista


decodifica :: ([Int] -> IO())
decodifica = decodificaMochila -- Cuadrado Magico decodificaCuadradoMagico -- Viajante decodificaViajante -- Sudoku decodificaSudoku


--Cuando trabajamos con cromosomas de tipo Permutation, solo podemos utilizar combinaciones y mutaciones que alteren las posiciones de los genes sin que el conjunto de valores cambie
--La suma de los porcentajes debe ser 100


itera :: IO [[Int]] -> TipoCromosoma -> Objetivo -> (Int,Int) -> Int -> Int -> [(String,Int)] -> ([Int] -> Double) -> IO [[Int]]
itera xs tipoCromosma Min valoresGenRange tamanoCromosoma poblacion porcentajeMezcla fitness = do
    lista <- xs
    let padres = seleccionElitistaMinimizar lista ((snd (porcentajeMezcla!!0))) fitness 
    let random = seleccionElitistaMaximizar lista (div ((100-(div ((snd (porcentajeMezcla!!0))*100) poblacion))*poblacion) 100) fitness -- hace la lista de los no seleccionados

    --Combinaciones y mutaciones

    comb1 <- ejecutaCombinacion1 padres tamanoCromosoma porcentajeMezcla
    comb2 <- ejecutaCombinacion2 padres porcentajeMezcla
    combCiclos <- ejecutaCombinacionCiclos padres tamanoCromosoma porcentajeMezcla
    mut1 <- ejecutaMutacion1Int padres tamanoCromosoma valoresGenRange porcentajeMezcla
    permutInter <- ejecutaPermutacionInter padres tamanoCromosoma porcentajeMezcla

    --En random metemos individuos al azar para no caer en óptimos locales, seleccionados de los que no han sido padres
    random <- seleccionAleatoria random ((snd (porcentajeMezcla!!6))) 
    return (padres++comb1++comb2++combCiclos++mut1++permutInter++random)

itera xs tipoCromosma Max valoresGenRange tamanoCromosoma poblacion porcentajeMezcla fitness = do
    lista <- xs
    let padres = seleccionElitistaMaximizar lista ((snd (porcentajeMezcla!!0))) fitness 
    let random = seleccionElitistaMinimizar lista (div ((100-(div ((snd (porcentajeMezcla!!0))*100) poblacion))*poblacion) 100) fitness -- hace la lista de los no seleccionados

    --Combinaciones y mutaciones
    comb1 <- ejecutaCombinacion1 padres tamanoCromosoma porcentajeMezcla
    comb2 <- ejecutaCombinacion2 padres porcentajeMezcla
    combCiclos <- ejecutaCombinacionCiclos padres tamanoCromosoma porcentajeMezcla
    mut1 <- ejecutaMutacion1Int padres tamanoCromosoma valoresGenRange porcentajeMezcla
    permutInter <- ejecutaPermutacionInter padres tamanoCromosoma porcentajeMezcla

    --En random metemos individuos al azar para no caer en óptimos locales, seleccionados de los que no han sido padres
    random <- seleccionAleatoria random ((snd (porcentajeMezcla!!6))) 
    return (padres++comb1++comb2++combCiclos++mut1++permutInter++random)



iteraciones ::  IO[[Int]] -> TipoCromosoma -> Objetivo -> Int -> Double -> (Int, Int) -> Int -> Int -> [(String,Int)] -> ([Int] -> Double) -> IO[Int]

iteraciones xs _ Max 0 _ _ _ _ _ fitness = do
    lista <- xs
    return (mejorMax fitness lista)

iteraciones xs _ Min 0 _ _ _ _ _ fitness = do
    lista <- xs
    return (mejorMin fitness lista)

iteraciones xs tipoCromosma Max it valorObjetivo valoresGenRange tamanoCromosoma poblacion porcentajeMezcla fitness= do
    lista <- xs
    if (fitness (mejorMax fitness lista)==valorObjetivo)
        then do
            return (mejorMax fitness lista)
        else do 
            r <- (iteraciones 
                    (itera xs tipoCromosma Max valoresGenRange tamanoCromosoma poblacion porcentajeMezcla fitness) 
                    tipoCromosma 
                    Max 
                    (it-1) 
                    valorObjetivo 
                    valoresGenRange
                    tamanoCromosoma 
                    poblacion 
                    porcentajeMezcla 
                    fitness)
            return r

iteraciones xs tipoCromosma Min it valorObjetivo valoresGenRange tamanoCromosoma poblacion porcentajeMezcla fitness = do
    lista <- xs
    if (fitness (mejorMin fitness lista)==valorObjetivo)
        then do
            return (mejorMin fitness lista)
        else do 
            r <- (iteraciones 
                    (itera xs tipoCromosma Min valoresGenRange tamanoCromosoma poblacion porcentajeMezcla fitness) 
                    tipoCromosma 
                    Min 
                    (it-1) 
                    valorObjetivo 
                    valoresGenRange 
                    tamanoCromosoma 
                    poblacion 
                    porcentajeMezcla 
                    fitness)
            return r



