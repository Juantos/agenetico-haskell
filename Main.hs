module Main where

import MainGenetico


linea = putStrLn "---------"
main :: IO ()
main = do
    print "-------------------------------------------"
    putStrLn "Introduce 'm' para ejecutar el problema de la mochila"
    putStrLn "Introduce 'v' para ejecutar el problema del viajante"
    putStrLn "Introduce 's' para ejecutar el problema del sudoku"
    putStrLn "Introduce 'c' para ejecutar el problema del cuadrado mágico"
    putStrLn "Introduce 'x' para salir del programa"
    chr <- getLine

    if (chr == "x") then do
        return ()
    else do
    
        if (chr == "m") then do
            linea
            putStrLn "Ejecutando mochila..."
            ejecutaMochila
            linea
            putStrLn "Introduce cualquier tecla para volver al menú principal"
            a <- getLine
            main
        else do
    
            if (chr == "v") then do
                linea
                putStrLn "Ejecutando viajante..."
                ejecutaViajante
                linea
                putStrLn "Introduce cualquier tecla para volver al menú principal"
                a <- getLine
                main
            else do

                if (chr == "s") then do
                    linea
                    putStrLn "Ejecutando sudoku..."
                    ejecutaSudoku
                    linea
                    putStrLn "Introduce cualquier tecla para volver al menú principal"
                    a <- getLine
                    main

                else do
                    if (chr == "c") then do
                        linea
                        putStrLn "Ejecutando cuadrado mágico..."
                        ejecutaCuadradoMagico
                        linea
                        putStrLn "Introduce cualquier tecla para volver al menú principal"
                        a <- getLine
                        main
                    else do
                        main