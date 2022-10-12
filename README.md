# agenetico-haskell
Algoritmo genético genérico en Haskell.
Por defecto trae ejemplos de knapsack problem, traveling salesman problem, sudokus y cuadrado mágico

Para compilar:

1.En primer lugar debemos acceder a la carpeta que contiene el proyecto descomprimido con el comando cd

2.Para compilar el proyecto escribir en línea de comandos del símbolo del sistema de windows:

ghc --make Main.hs -o agenetico

3.Posteriormente para ejecutarlo debemos escribir:

./agenetico

4.Una vez hecho esto se imprimirán por pantalla las instrucciones que indican como proceder, introduciendo "c", "s", "m" y "v" respectivamente ejecutaremos 
el problema del cuadrado mágico, del sudoku, de la mochila y del viajante respectivamente.

5.Para abandonar el programa, introducir "x".
