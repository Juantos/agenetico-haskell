module Generador(
    generaAleatorioIO
) where

import System.Random
import System.IO.Unsafe

generaAleatorioIO :: IO Int
generaAleatorioIO = randomIO

generaListaAleatoria :: Int -> [Double]
generaListaAleatoria seed = randoms (mkStdGen seed)

numeroAleatorio :: Int -> Double
numeroAleatorio seed = head (generaListaAleatoria seed)

-- a randomly chosen, program-scoped constant from the range [0 .. 9]            
--c :: Int
--c = unsafePerformIO (getStdRandom (randomR (0, 9)))
