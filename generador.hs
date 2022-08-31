module Generador(
    generaAleatorioIO
) where

import System.Random
import System.IO.Unsafe

--recibe un número aleatorio d y devuelve otro en rango [0, n)
ceroAEnteroRango :: Int -> Double -> Int
ceroAEnteroRango n d = floor (d * 1000) `mod` n

--genera un número aleatorio que usaremos como seed en otras funciones
generaAleatorioIO :: IO Int
generaAleatorioIO = randomIO

--genera una lista aleatoria utilizando un seed
generaListaAleatoria :: Int -> [Double]
generaListaAleatoria seed = randoms (mkStdGen seed)

--genera un número aleatorio entre 0 y n dado un seed
numeroAleatorio :: Int -> Int -> Int
numeroAleatorio seed n = ceroAEnteroRango n (head (generaListaAleatoria seed))

-- a randomly chosen, program-scoped constant from the range [0 .. 9]            
--c :: Int
--c = unsafePerformIO (getStdRandom (randomR (0, 9)))

--toma una seed, y genera una lista de longitud l en rango de [0, n)
generaEstadoInicial :: Int -> Int -> Int -> [Int]
generaEstadoInicial seed l n = take l (map (ceroAEnteroRango n) (generaListaAleatoria seed))

main = do
  g <- newStdGen
  print . take 10 $ (randomRs ('a', 'z') g)

