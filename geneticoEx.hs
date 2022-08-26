--PARÁMETROS DEL ALGORITMO GENÉTICO
numIteraciones :: Int --criterio de parada del algoritmo genético 
numIteraciones = 10000

objetivo :: String --objetivo de la función , valores posibles: "MAX" "MIN"

poblacion :: Int --número de cromosomas que se evalúan en cada iteración
poblacion = 100

tipoCromosoma :: String --valores posibles "ValuesInRange" "Permutation"
tipoCromosoma = "Permutation"

tamanoCromosoma :: Int --length de la lista que representa al cromosoma
tamanoCromosoma = 9  --valores para el ejemplo del cuadrado mágico

valoresGenRange :: (Int,Int) --valor mínimo y máximo que puede tomar un gen en el cromosoma (para ValuesInRange)
valoresGenRange = (0,100)

valoresGenPermutation :: [Int]
valoresGenPermutation = [1..9] --valores para el ejemplo del cuadrado mágico

porcentajeMezcla :: [(String,Int)] --Describe que mutaciones y combinaciones utilizar y en qué porcentaje
porcentajeMezcla = [("padres",20),("comb1",0),("comb2",0),("combCiclos",70),("mut1",0),("mutInter",5),("mutInser",5)]
--Cuando trabajamos con cromosomas de tipo Permutation, solo podemos utilizar combinaciones y mutaciones que alteren las posiciones de los genes sin que el conjunto de valores cambie
--La suma de los porcentajes debe ser 100