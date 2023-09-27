module Main where

import AllEquals
import Json
import SchemyInterpreter
import Data.List.Split
import Data.Map (fromList)
import Schemy


main :: IO ()
main = do
  putStrLn "Ingresa una expresión Schemy:"
  line <- getLine
  case parseSchemyExp line of
    Left err -> putStrLn ("Error al parsear la expresión: " ++ show err)
    Right exp -> do
      putStrLn $ "Expresión: " ++ show exp
      let result = eval basicEnv exp
      putStrLn $ "Resultado: " ++ show result
  main
