


--COMBINACIONES
--------------------------------------------------------------------------------------------------------------------------
combinacion1 :: [a] -> [a] -> Int -> [a] --Recibe dos cromosomas y la posicion a partir de la cual termina el primero y comienza el segundo
combinacion1 xs ys i = combinacionAux xs ys i []

combinacion1Aux :: [a] -> [a] -> Int -> [a] -> [a]
combinacion1Aux [x] [y] i zs
    | i==0 = zs ++ [y]
    | otherwise = zs ++ [x]
combinacionAux (x:xs) (y:ys) i zs 
    |i==0 = combinacion1Aux xs ys 0 (zs++[y])
    |otherwise = combinacion1Aux xs ys (i-1) (zs++[x])
--------------------------------------------------------------------------------------------------------------------------
combinacion2 :: [a] -> [a] -> [a]     --Recibe dos cromosomas y genera uno nuevo que alterna un gen de cada uno de los padres
combinacion2 xs ys = combinacion2Aux xs ys [] 0
combinacion2Aux :: [a] -> [a] -> [a] -> Int -> [a]
combinacion2Aux [] [] zs _ =  zs
combinacion2Aux (x:xs) (y:ys) zs 0 = combinacion2Aux xs ys (zs++[x]) 1 
combinacion2Aux (x:xs) (y:ys) zs 1 = combinacion2Aux xs ys (zs++[y]) 0 
--------------------------------------------------------------------------------------------------------------------------
--combinacionCiclos :: [a] -> [a] -> Int -> [a] --Recibe dos cromosomas y devuelve uno nuevo utilizando el cruce basado en ciclos (para cromosomas en los que los elementos no pueden repetirse)
--combinacionCiclos 
     

--MUTACIONES
--------------------------------------------------------------------------------------------------------------------------
mutacion1 :: [a] -> Int -> a -> [a]
mutacion1 xs pos num = (take pos xs) ++ [num] ++ (reverse (take ((length xs)-(pos+1)) (reverse xs))) 
--num es un numero aleatorio introducido como parametro de la función 
--------------------------------------------------------------------------------------------------------------------------
permutacioninter :: [a] -> Int -> Int -> [a]   --Recibe el cromosoma y las dos posiciones a intercambiar (importante introducir los numeros en orden)
permutacioninter xs pos1 pos2 = (take pos1 xs) ++ [xs !! pos2] ++ (interludio xs pos1 pos2) ++ [xs !! pos1] ++ (drop (pos2+1) xs)

interludio :: [a] -> Int -> Int -> [a]
interludio xs pos1 pos2 = take (pos2-(pos1+1)) (drop (pos1+1) xs)
--------------------------------------------------------------------------------------------------------------------------
permutacioninser :: [a] -> Int -> Int -> [a]   --Recibe el cromosoma la posición donde insertar el gen y la posicion del gen a insertar
permutacioninser xs pos1 pos2 = (take (pos1+1) xs) ++ [xs !! pos2] ++ (drop (pos1+1) (unget pos2 xs))

unget :: Int -> [a] -> [a]
unget pos xs = (take pos xs) ++ (reverse (take ((length xs)-(pos+1)) (reverse xs)))
--------------------------------------------------------------------------------------------------------------------------
--permutacionmezcla :: [a] -> Int -> --Esta creo que mejor no hacerla porque tiene una componente aleatoria que tal y como es haskell veo complicada
--permutacionmezcla = 
--------------------------------------------------------------------------------------------------------------------------

--FUNCIONES AUXILIARES
--posEnLista :: a -> [a] -> Int --Funcion auxiliar que recibe un elemento y una lista que lo contiene y devuelve la posición que ocupa dicho elemento en la lista
--posEnLista e xs = 
--    if noElem e xs 
--        then error "El elemento no se encuentra en la lista"
--    else length (take (/=e) xs)
-- Preguntar por predicados a teacher
        