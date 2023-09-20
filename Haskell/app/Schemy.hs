module Schemy where

import Data.Map.Strict (Map, fromList, toList, (!))
import Text.Parsec
  ( ParseError,
    char,
    choice,
    digit,
    letter,
    many1,
    option,
    parse,
    spaces,
    string,
    try,
    (<|>),
  )
import Text.Parsec.String (Parser)

import SchemyInterpreter

basicEnv :: SchemyEnv
basicEnv =
  fromList
    [ ("pi", SchemyNumber pi)
    ]

binaryOperations :: Map String (SchemyExp -> SchemyExp -> SchemyExp)
binaryOperations =
  fromList
    [ ("+", SchemyAdd),
      ("*", SchemyMult),
      ("==", SchemyEqual),
      ("==", SchemyEqual),
      ("<=", SchemyLessThanOrEqual),
      ("&&", SchemyAnd),
      ("||", SchemyOr),
      (">", SchemyGreaterThan),
      (">=", SchemyGreaterThanOrEqual),
      ("<", SchemyLessThan),
      ("!=", SchemyNotEqual)
    ]

unaryOperations :: Map String (SchemyExp -> SchemyExp)
unaryOperations =
  fromList
    [ ("!", SchemyNot)
    ]

-- Inicio no modificar
parseNumber :: Parser SchemyExp
parseNumber = do
    sign <- option "" (string "-")
    intPart <- many1 digit
    decimalPart <- option "" $ do
        char '.'
        fracPart <- many1 digit
        return ('.' : fracPart)
    let numStr = sign ++ intPart ++ decimalPart
    return (SchemyValue (SchemyNumber (read numStr :: Double)))
    
parseBool :: Parser SchemyExp
parseBool = do
  bool <- choice [try (string "true" >> return True), try (string "false" >> return False)]
  return (SchemyValue (SchemyBool bool))

parseSymbol :: Parser SchemyExp
parseSymbol = do
  symbol <- many1 (letter <|> digit)
  return (SchemySymbol symbol)

parseBinarySExp :: Parser SchemyExp
parseBinarySExp = do
  char '('
  spaces
  op <- choice (map (string . fst) (toList binaryOperations))
  spaces
  expr1 <- try parseBinarySExp <|> try parseUnarySExp <|> try parseNumber <|> try parseBool <|> try parseSymbol
  spaces
  expr2 <- try parseBinarySExp <|> try parseUnarySExp <|> try parseNumber <|> try parseBool <|> try parseSymbol
  spaces
  char ')'
  return ((binaryOperations ! op) expr1 expr2)

parseUnarySExp :: Parser SchemyExp
parseUnarySExp = do
  char '('
  spaces
  op <- choice (map (string . fst) (toList unaryOperations))
  spaces
  expr <- try parseBinarySExp <|> try parseUnarySExp <|> try parseNumber <|> try parseBool <|> try parseSymbol
  spaces
  char ')'
  return ((unaryOperations ! op) expr)

parseSchemy :: Parser SchemyExp
parseSchemy = try parseBinarySExp <|> try parseUnarySExp <|> try parseNumber <|> try parseBool <|> try parseSymbol

parseSchemyExp :: String -> Either ParseError SchemyExp
parseSchemyExp = parse parseSchemy ""

-- Fin no modificar