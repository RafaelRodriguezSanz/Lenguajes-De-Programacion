module Json where

import Data.List (intercalate)

data JSONValue
    = JSONNull
    | JSONBool Bool
    | JSONNumber Double
    | JSONString String
    | JSONArray [JSONValue]
    | JSONObject [(String, JSONValue)]
    deriving (Eq)

instance Show JSONValue where
    show JSONNull = "null"
    show (JSONBool b) = show b
    show (JSONNumber n) = show n
    show (JSONString s) = show s
    show (JSONArray arr) = "[" ++ intercalate "," (map show arr) ++ "]"
    show (JSONObject obj) = "{" ++ intercalate "," (map showPair obj) ++ "}"
      where
        showPair (key, val) = show key ++ ":" ++ show val