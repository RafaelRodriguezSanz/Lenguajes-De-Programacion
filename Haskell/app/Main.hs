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
    let env = fromList [("pi", SchemyNumber (3.141592653589793)), ("e", SchemyNumber (exp 1.0))]
    let e1 = SchemyMult (SchemySymbol "pi") (SchemySymbol "pi")
    let e2 = SchemySymbol "e"
    let e3 = SchemyAdd (SchemyValue (SchemyNumber 5)) (SchemyValue (SchemyNumber 3))
    let e4 = SchemyEqual (SchemyValue (SchemyNumber 5)) (SchemyValue (SchemyNumber 3))
    let e5 = SchemyLessThanOrEqual (SchemyValue (SchemyNumber 5)) (SchemyValue (SchemyNumber 3))
    let e6 = SchemyNot (SchemyValue (SchemyBool True))
    let e7 = SchemyAnd (SchemyValue (SchemyBool True)) (SchemyValue (SchemyBool False))
    let e8 = SchemyGreaterThan (SchemyValue (SchemyNumber 5)) (SchemyValue (SchemyNumber 3))
    let e9 = SchemyGreaterThanOrEqual (SchemyValue (SchemyNumber 5)) (SchemyValue (SchemyNumber 3))
    let e10 = SchemyLessThan (SchemyValue (SchemyNumber 5)) (SchemyValue (SchemyNumber 3))
    let e11 = SchemyNotEqual (SchemyValue (SchemyNumber 5)) (SchemyValue (SchemyNumber 3))
    let e12 = SchemyOr (SchemyValue (SchemyBool True)) (SchemyValue (SchemyBool False))

    putStrLn $ "Resultado de e1: " ++ show (eval env e1)
    putStrLn $ "Resultado de e2: " ++ show (eval env e2)
    putStrLn $ "Resultado de e3: " ++ show (eval env e3)
    putStrLn $ "Resultado de e4: " ++ show (eval env e4)
    putStrLn $ "Resultado de e5: " ++ show (eval env e5)
    putStrLn $ "Resultado de e6: " ++ show (eval env e6)
    putStrLn $ "Resultado de e7: " ++ show (eval env e7)
    putStrLn $ "Resultado de e8: " ++ show (eval env e8)
    putStrLn $ "Resultado de e9: " ++ show (eval env e9)
    putStrLn $ "Resultado de e10: " ++ show (eval env e10)
    putStrLn $ "Resultado de e11: " ++ show (eval env e11)
    putStrLn $ "Resultado de e12: " ++ show (eval env e12)