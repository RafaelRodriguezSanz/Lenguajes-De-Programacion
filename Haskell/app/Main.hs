module Main where

import MiModulo
import Json
import Data.List.Split

main :: IO ()
main = do
    --putStrLn "Please, enter csv a csv list of values:"
    --input <- getLine
    --if input == "exit"
    --    then putStrLn "Hasta la vista, Baby! 😎"
    --    else do
    --        let valores = splitOn "," input
    --        let resultado = if allEqual valores then "They are all equals" else "They are not all equals"
    --        putStrLn ("Result: " ++ resultado)
    --        main
    let crearObjeto = JSONObject
            [ ("id", JSONNumber 123),
            ("value", JSONString "admin")
            ]
    putStrLn (show crearObjeto)