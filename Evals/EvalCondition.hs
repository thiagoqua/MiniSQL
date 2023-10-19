module Evals.EvalCondition (verifCond') where

import AST
import Text.Read (readMaybe)
import Data.List (isInfixOf)
import Extra.Helpers
import System.Directory.Internal.Prelude (exitFailure)

-- Funcion para verificar que se cumpla la condicion en un registro

verifCond' reg (CAnd cond1 cond2) fields = do
    result1 <- verifCond' reg cond1 fields
    result2 <- verifCond' reg cond2 fields
    return (result1 && result2)

verifCond' reg (COr cond1 cond2) fields = do
    result1 <- verifCond' reg cond1 fields
    result2 <- verifCond' reg cond2 fields
    return (result1 || result2)

verifCond' reg (CNot cond) fields = do
    result <- verifCond' reg cond fields
    return (not result)

verifCond' reg (Exp op name primalType) fields =
    if columnExists name fields then
        case findDataType fields name of
            Just typeStr -> 
                if compareTypes primalType typeStr then
                    Right $ evalCond op (splitColumns reg name fields) primalType
                else 
                    Right False
            Nothing -> 
                Right False
    else 
        Left ("\nNo se encontró el nombre de la columna '" ++ name ++ "' en 'fields'.")

-- Funcion para evaluar la condicion
evalCond :: Op -> String -> PrimalType -> Bool
evalCond Eq colValue (S value) = colValue == value
evalCond Eq colValue (I value) = colValue == show value
evalCond Eq colValue (B value) = colValue == show value

evalCond Bt colValue (I value) = case readMaybe colValue of
                                    Just number -> number > value
                                    Nothing -> False

evalCond Bte colValue (I value) = case readMaybe colValue of
                                    Just number -> number >= value
                                    Nothing -> False

evalCond Lt colValue (I value) = case readMaybe colValue of
                                    Just number -> number < value
                                    Nothing -> False

evalCond Lte colValue (I value) = case readMaybe colValue of
                                    Just number -> number <= value
                                    Nothing -> False

evalCond Neq colValue (S value) = colValue /= value
evalCond Neq colValue (I value) = colValue /= show value
evalCond Neq colValue (B value) = colValue /= show value


-- Funciones para encontrar el tipo de dato de una columna en la primera línea del archivo

-- Divide por | y mapea cada tupla
findDataType str campo =
    let campos = map parseCampo (splitOn '|' str)
    in lookup campo campos

parseCampo str =
    -- Sacar parentesis, dividir por coma y mapear cada string
    let elems = map (filter (`notElem` "()")) (splitOn ',' str)
    -- "1" representa la posicion del tipo de dato
    in (head elems, elems !! 1)

-- Funcion para encontrar el valor de una columna

splitColumns reg name fields = do
    -- Ej.: ["(12,integer,)","(18,integer,)"]
    let columnsValues = splitOn '|' reg
    -- Ej.: ["(id,integer,)","(edad,integer,)"]
    let columnsNames = splitOn '|' fields
    -- Obtiene el indice de la columna que se quiere buscar en columnsNames
    let columnIndex = matchColumnName columnsNames name 0
    -- Mediante el indice, se obtiene el valor de la columna -> Ej.: "(18,integer,)"
    parseValue (columnsValues !! columnIndex)


-- Funciones auxiliares de splitColumns

matchColumnName (x:xs) name index = 
    if name `isInfixOf` x
        then index
        else matchColumnName xs name (index + 1)

parseValue column = let elems = map (filter (`notElem` "()")) (splitOn ',' column)
                    in head elems