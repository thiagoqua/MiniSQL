
module Evals.EvalCondition (verifCond') where

import AST
    ( Cond(Exp, CAnd, COr, CNot),
      Field(..),
      Name,
      Op(..),
      PrimalType(..) )
import Data.List (isInfixOf)
import Extra.Helpers ( columnExists, compareTypes )
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
            Just field ->
                if compareTypes primalType field 
                then
                    Right $ evalCond op (findColumn reg name fields) primalType
                else
                    Left "Los tipos de los valores a comparar en la condición no son iguales."
            Nothing ->
                Right False
    else
        Left ("\nNo se encontró el nombre de la columna '" ++ name ++ "' en la tabla.")

-- Funcion para evaluar la condicion
evalCond :: Op -> PrimalType -> PrimalType -> Bool
evalCond Eq (I regValue) (I condValue) = regValue == condValue
evalCond Eq (S regValue _) (S condValue _) = regValue == condValue
evalCond Eq (B regValue) (B condValue) = regValue == condValue

evalCond Bt (I regValue) (I condValue) = regValue > condValue
evalCond Bt (S regValue _) (S condValue _) = regValue > condValue
evalCond Bt (B regValue) (B condValue) = regValue > condValue

evalCond Bte (I regValue) (I condValue) = regValue >= condValue
evalCond Bte (S regValue _) (S condValue _) = regValue >= condValue
evalCond Bte (B regValue) (B condValue) = regValue >= condValue

evalCond Lt (I regValue) (I condValue) = regValue < condValue
evalCond Lt (S regValue _) (S condValue _) = regValue < condValue
evalCond Lt (B regValue) (B condValue) = regValue < condValue

evalCond Lte (I regValue) (I condValue) = regValue <= condValue
evalCond Lte (S regValue _) (S condValue _) = regValue <= condValue
evalCond Lte (B regValue) (B condValue) = regValue <= condValue

evalCond Neq (I regValue) (I condValue) = regValue /= condValue
evalCond Neq (S regValue _) (S condValue _) = regValue /= condValue
evalCond Neq (B regValue) (B condValue) = regValue /= condValue

-- Funciones para encontrar el tipo de dato de una columna en la primera línea del archivo

findDataType :: [Field] -> Name -> Maybe Field
findDataType [] _ = Nothing
findDataType (f:fields) campo =
    case f of
        String name _ -> if name == campo then Just f else findDataType fields campo
        Integer name -> if name == campo then Just f else findDataType fields campo
        Bool name -> if name == campo then Just f else findDataType fields campo


-- Funcion para encontrar el valor de una columna

--          reg             name            fields
-- [(I 18),(S "thiago")]     id     [Integer id, String nombre 20]

findColumn :: [PrimalType] -> String -> [Field] -> PrimalType
findColumn reg name fields = 
    -- Obtiene el indice de la columna que se quiere buscar en los fields (primera linea)
    let columnIndex = matchColumnName fields name 0
    -- Mediante el indice, se obtiene el valor de la columna en el registro
    in (reg !! columnIndex)

-- Funciones auxiliares de findColumn

--                 fields                          campo
--          [Integer id, String nombre 20]          id     

matchColumnName :: [Field] -> String -> Int -> Int
matchColumnName (f:fields) campo index =
    case f of
        String name _ -> if name == campo then index else matchColumnName fields campo (index + 1)
        Integer name -> if name == campo then index else matchColumnName fields campo (index + 1)
        Bool name -> if name == campo then index else matchColumnName fields campo (index + 1)
