module Generador(
    ceroAEnteroRango,
    generaAleatorioIO,
    randIntRango,
    generaListaAleatoria,
    numeroAleatorio,
    generaEstadoInicial,
    generaPoblacion,
    splitList,
    split2
) where

import System.Random
import System.IO.Unsafe
import Data.List

--recibe un número aleatorio d y devuelve otro en rango [0, n)
ceroAEnteroRango :: Int -> Double -> Int
ceroAEnteroRango n d = floor (d * 1000) `mod` n

--genera un número aleatorio que usaremos como seed en otras funciones
generaAleatorioIO :: IO Int
generaAleatorioIO = randomIO

--genera un número aleatorio en un rango
randIntRango :: Int -> Int -> IO Int
randIntRango x y = getStdRandom (randomR (x,y))

--genera una lista aleatoria utilizando un seed
generaListaAleatoria :: Int -> [Double]
generaListaAleatoria seed = randoms (mkStdGen seed)

--genera un número aleatorio entre 0 y n dado un seed
numeroAleatorio :: Int -> Int -> Int
numeroAleatorio seed n = ceroAEnteroRango n (head (generaListaAleatoria seed))

-- a randomly chosen, program-scoped constant from the range [0 .. 9]            
--c :: Int
--c = unsafePerformIO (getStdRandom (randomR (0, 9)))

-- genera una lista de longitud l en rango de [n, m]
generaEstadoInicial :: Int -> Int -> Int -> IO [Int]
--generaEstadoInicial seed l n = take l (map (ceroAEnteroRango n) (generaListaAleatoria seed))
generaEstadoInicial _ _ 0 = return [] 
generaEstadoInicial n m l = do
    r <- randIntRango n m
    rs <- generaEstadoInicial n m (l-1)
    return (r:rs)



--tomando una seed, la longitud del cromosoma, el número de individuos que queremos y el rango en el que queremos que esté cada cromosoma,
--genera una población inicial
--generaPoblacion :: Int -> Int -> Int -> Int -> [[Int]]
--generaPoblacion seed l l2 n     = splitList (generaEstadoInicial seed (l2*l) n) l
generaPoblacion :: Int -> Int -> Int -> Int -> IO [[Int]]
generaPoblacion _ _ _ 0 = return []
generaPoblacion n m l i = do
    r <- generaEstadoInicial n m l
    rs <- generaPoblacion n m l (i-1)
    return (r:rs)

--toma una lista y un número n, devuelve la lista fragmentada en n sublistas
splitList :: [a] -> Int -> [[a]]
splitList xs n = split2 xs (div (length xs) n)

split2 :: [a] -> Int -> [[a]]
split2 [] _ = []
split2 xs l = (take l xs) : split2 (drop l xs) l
