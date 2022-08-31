module Main where

import Generador
import AGenetico
import System.Random
import Control.Monad (replicateM)

fitnessMochila :: [Int] -> [Int] -> [Int] -> Int -> Int
fitnessMochila xs bs cs w
    | sum (multiplicaListas xs cs) <= w     = sum (multiplicaListas xs bs)
    | otherwise                             = sum (multiplicaListas xs bs) * (-1000)

multiplicaListas :: [Int] -> [Int] -> [Int]
multiplicaListas = zipWith (*)


main = do 
        a <- generaPoblacion 0 10 10 10
        print a
