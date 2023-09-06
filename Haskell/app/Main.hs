module Main where

import MiModulo
import Data.List.Split

main :: IO ()
main = do
    putStrLn "Por favor, ingresa una lista de valores separados por comas:"
    input <- getLine
    if input == "salir"
        then putStrLn "Hasta la vista, Baby! 😎"
        else do
            let valores = splitOn "," input
            let resultado = if allEqual valores then "Son todos iguales" else "No son todos iguales"
            putStrLn ("El resultado es: " ++ resultado)
            main