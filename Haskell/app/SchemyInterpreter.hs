module SchemyInterpreter where
    
import Data.Map (Map)
import qualified Data.Map as Map

type SchemyEnv = Map String SchemyValue

data SchemyExp
    = SchemyValue SchemyValue
    | SchemyAdd SchemyExp SchemyExp
    | SchemyMult SchemyExp SchemyExp
    | SchemySymbol String
    | SchemyEqual SchemyExp SchemyExp
    | SchemyLessThanOrEqual SchemyExp SchemyExp
    | SchemyNot SchemyExp
    | SchemyAnd SchemyExp SchemyExp
    | SchemyOr SchemyExp SchemyExp
    | SchemyGreaterThan SchemyExp SchemyExp
    | SchemyGreaterThanOrEqual SchemyExp SchemyExp
    | SchemyLessThan SchemyExp SchemyExp
    | SchemyNotEqual SchemyExp SchemyExp
    deriving (Eq)

data SchemyValue
    = SchemyNumber Double
    | SchemyBool Bool
    deriving (Eq)

eval :: SchemyEnv -> SchemyExp -> SchemyValue
eval env (SchemyValue v) = v
eval env (SchemyAdd a b) = SchemyNumber (left (eval env a) + left (eval env b))
eval env (SchemyMult a b) = SchemyNumber (left (eval env a) * left (eval env b))
eval env (SchemyEqual a b) = SchemyBool (eval env a == eval env b)
eval env (SchemyLessThanOrEqual a b) = SchemyBool (left (eval env a) <= left (eval env b))
eval env (SchemyAnd a b) = case (eval env a, eval env b) of
    (SchemyBool x, SchemyBool y) -> SchemyBool (x && y)
    _ -> error "Operadores lógicos solo pueden ser aplicados a valores booleanos o numéricos"
eval env (SchemyOr a b) = case (eval env a, eval env b) of
    (SchemyBool x, SchemyBool y) -> SchemyBool (x || y)
    _ -> error "Operadores lógicos solo pueden ser aplicados a valores booleanos o numéricos"
eval env (SchemyNot a) = case eval env a of
    SchemyBool x -> SchemyBool (not x)
    _ -> error "Operador lógico 'not' solo puede ser aplicado a valores booleanos o numéricos"
eval env (SchemyGreaterThan a b) = SchemyBool (left (eval env a) > left (eval env b))
eval env (SchemyGreaterThanOrEqual a b) = SchemyBool (left (eval env a) >= left (eval env b))
eval env (SchemyNotEqual a b) = SchemyBool (eval env a /= eval env b)
eval env (SchemyLessThan a b) = SchemyBool (left (eval env a) < left (eval env b))
eval env (SchemySymbol s) = case Map.lookup s env of
    Just value -> value
    Nothing    -> error ("Símbolo no definido: " ++ s)


left :: SchemyValue -> Double
left (SchemyNumber d) = d
left _ = error "Se esperaba un SchemyNumber"

right :: SchemyValue -> Bool
right (SchemyBool b) = b
right _ = error "Se esperaba un SchemyBool"

instance Show SchemyExp where
    show (SchemySymbol s) = s
    show (SchemyAdd a b) = "(" ++ show a ++ ")" ++ " + " ++ "(" ++ show b ++ ")" 
    show (SchemyMult c d) = "(" ++ show c ++  ")" ++ " * " ++ "(" ++ show d ++ ")" 
    show (SchemyEqual a b) = "(" ++ show a ++  ")" ++ " == " ++ "(" ++ show b ++ ")"
    show (SchemyLessThanOrEqual a b) = "(" ++ show a ++  ")" ++ " <= " ++ "(" ++ show b ++ ")"
    show (SchemyNot a) = "not (" ++ show a ++ ")"
    show (SchemyAnd a b) = "(" ++ show a ++  ") && (" ++ show b ++ ")"
    show (SchemyGreaterThan a b) = "(" ++ show a ++  ")" ++ " > " ++ "(" ++ show b ++ ")"
    show (SchemyGreaterThanOrEqual a b) = "(" ++ show a ++  ")" ++ " >= " ++ "(" ++ show b ++ ")"
    show (SchemyLessThan a b) = "(" ++ show a ++  ")" ++ " < " ++ "(" ++ show b ++ ")"
    show (SchemyNotEqual a b) = "(" ++ show a ++  ")" ++ " != " ++ "(" ++ show b ++ ")"
    show (SchemyOr a b) = "(" ++ show a ++  ") || (" ++ show b ++ ")"

    
instance Show SchemyValue where
    show (SchemyNumber n) = show n
    show (SchemyBool b) = show b