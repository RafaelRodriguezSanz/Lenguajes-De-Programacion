module Main where

import AllEquals
import Json
import SchemyInterpreter
import Data.List.Split
import Data.Map (fromList)

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
    --let crearObjeto = JSONObject
    --        [ ("id", JSONNumber 123),
    --        ("value", JSONString "admin")
    --        ]
    --putStrLn (show crearObjeto)
    
        let uno = SchemyNumber 1
        let dos = SchemyNumber 2
        let exp = SchemyAdd uno dos
        let resultado = eval (fromList []) exp
        putStrLn $ "El resultado de la suma es: " ++ show resultado