module Evals.EvalInsert (evalInsert) where

import System.Directory ( doesFileExist )
import System.FilePath ( (<.>), (</>) )
import System.IO
    ( hClose,
      hSeek,
      hPutStr,
      SeekMode(SeekFromEnd)
    )

import Extra.Helpers
    ( compareTypes, formatData, getValue, parseFields, openTable )

import AST ( Field(String), PrimalType(S) )

evalInsert tableName newData currentDatabase = do
    let tablePath = currentDatabase </> tableName <.> "txt"
    tableExists <- doesFileExist tablePath
    -- Revisar si existe la tabla
    if tableExists
        then do
            (stream,fieldsAsStr) <- openTable tablePath
            case parseFields fieldsAsStr of 
                Right fields -> do
                        -- Revisar si el contenido es valido
                        isDataValid <- validateData fields newData
                        if isDataValid
                            then do
                                let dataToInsert = formatData newData
                                -- Posiciona el puntero al final de los registros existente
                                hSeek stream SeekFromEnd 0
                                -- Inserta los registros nuevos
                                hPutStr stream dataToInsert
                                hClose stream
                                putStrLn "Registro añadido exitosamente."
                            else hClose stream
                Left _ -> putStrLn "Hubo un error interno"
        else putStrLn $ "La tabla '" ++ tableName ++ "' no existe en la base de datos '" ++ currentDatabase ++ "'." 

validateData _ [] = return True
validateData fields (reg:regs) = do
    if length fields /= length reg
        then do
            putStrLn "La cantidad de datos a insertar es distinta a la cantidad de columnas de la tabla."
            return False
        else do
            isValidColumnType <- validateColumnTypes fields reg 0
            if isValidColumnType
                then validateData fields regs
                else return False

-- Validar si el tipo de las columnas coincide con los tipos de los datos a ingresar
validateColumnTypes fields reg index = do
    if index == length fields
        then
            return True
        else do
            let colType = fields !! index
            if compareTypes (reg !! index) colType
                then
                    case colType of
                        String _ len -> 
                            if validateColumnLength fields reg index
                                then do
                                        putStrLn $ "La longitud del string supera el limite establecido de " ++ show len ++ " caracteres en el valor '" ++ getValue (reg !! index) ++ "'."
                                        return False
                                else validateColumnTypes fields reg (index + 1)
                        _ -> validateColumnTypes fields reg (index + 1)
                else do
                    putStrLn $ "El tipo de dato del valor '" ++ getValue (reg !! index) ++ "' no coincide con el de la columna."
                    return False

-- Validar si la longitud del string definido en la columna coincide con la longitud del dato a ingresar
-- retorna TRUE si la longitud del string a insertar es mayor o igual que la definida en la tabla
validateColumnLength fields reg index = do
    let strToInsert = reg !! index
    case fields !! index of
        String _ lenColumn -> case strToInsert of
                            S _ len -> len > lenColumn
                            _ -> False    
        _ -> False
