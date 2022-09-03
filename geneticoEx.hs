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
porcentajeMezcla = [("padres",20),("comb1",1),("comb2",1),("combCiclos",70),("mut1",1),("mutInter",5),("mutInser",5)]

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
    random <- randIntRango 0 (poblacion-1)
    random2 <- randIntRango (fst valoresGenRange) (snd valoresGenRange)
    let padres = {-if (mSeleccion == Elitista) then -} seleccionElitistaMaximizar lista ((snd (porcentajeMezcla!!0))) fitnessCuadradoMagico 
        --else seleccionRuleta xs ((snd (porcentajeMezcla!!0))*(poblacion/100)) fitnessCuadradoMagico 
    let comb1 = [combinacion1 (padres!!random) (padres!!random) random | x <- [1..((snd (porcentajeMezcla!!1)))]]
    let comb2 = [combinacion2 (padres!!random) (padres!!random)| x <- [1..((snd (porcentajeMezcla!!2)))]]
    let combCiclos = [combinacionCiclos (padres!!random) (padres!!random) random | x <- [1..((snd (porcentajeMezcla!!3)))]]
    let mut1 = [mutacion1 (padres!!random) random random2 | x <- [1..((snd (porcentajeMezcla!!4)))]]
    let mutInter = [mutacion1 (padres!!random) random random | x <- [1..((snd (porcentajeMezcla!!5)))]]
    let mutInser = [mutacion1 (padres!!random) random random | x <- [1..((snd (porcentajeMezcla!!6)))]]
    return (padres++comb1++comb2++combCiclos++mut1++mutInter++mutInser)

--prueba: itera (generaPoblacionPermutation [1..9] 100) Permutation Min
