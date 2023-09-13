module SchemyInterpreter where
    
import qualified Data.Map as Map

data SchemyExp
    = SchemyNumber Double
    | SchemyAdd SchemyExp SchemyExp
    | SchemyMult SchemyExp SchemyExp
    | SchemySymbol String
    deriving (Eq)

eval :: (Map String Double) -> SchemyExp -> Double
eval _ (SchemyNumber n) = n
eval env (SchemyAdd a b) = eval env a + eval env b
eval env (SchemyMult a b) = eval env a * eval env b
eval env (SchemySymbol s) = case Map.lookup s env of
    Just value -> value
    Nothing    -> error ("Símbolo no definido: " ++ s)

instance Show SchemyExp where
    show (SchemyNumber n) = show n 
    show (SchemySymbol s) = show s 
    show (SchemyAdd a b) = "(" ++ show a ++ ")" ++ " + " ++ "(" ++ show b ++ ")" 
    show (SchemyMult c d) = "(" ++ show c ++  ")" ++ " * " ++ "(" ++ show d ++ ")" 
    show (SchemyMult c d) = "(" ++ show c ++  ")" ++ " * " ++ "(" ++ show d ++ ")" 