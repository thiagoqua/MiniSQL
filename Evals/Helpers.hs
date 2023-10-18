module Evals.Helpers where

import AST
import Data.List (isInfixOf)

-- chequea que la columna name exista en la definición de la tabla
columnExists name fields = 
    let columns = splitOn '|' fields
    in columnExists' name columns

columnExists' _ [] = False
columnExists' name (col:cols) = name == extractValue col || columnExists' name cols

-- extrae de la definición de un valor de la bdd (ej. "(hernestino,string,10)") 
-- su valor (ej. "hernestino")
extractValue col = takeWhile (/= ',') (tail col)

-- Función para comparar tipos de datos
compareTypes :: PrimalType -> String -> Bool
compareTypes (S _) "string" = True
compareTypes (I _) "integer" = True
compareTypes (B _) "bool" = True
compareTypes _ _ = False

splitOn _ [] = []
splitOn delimiter list = let (first, rest) = break (== delimiter) list
                         in first : case rest of
                             [] -> []
                             (_:xs) -> splitOn delimiter xs

-- obtenemos los indices de la posicion del nombre de las columnas (de la primera linea)
findIndexes _ [] = []
findIndexes fields ((col,_):cols) = do
    idx <- findColumnPosition col fields 0
    idx : findIndexes fields cols

findColumnPosition name fields idx = do
    if length fields == idx
        then return (-1)  -- no es necesario
        else do
            let columns = splitOn '|' fields    -- ["id,integer", "name,string"]
            if name == extractValue (columns !! idx)
                then return idx
                else findColumnPosition name fields (idx + 1)