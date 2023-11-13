module Evals.EvalInsert (evalInsert) where

import Text.Read (readMaybe)
import System.Directory ( doesFileExist )
import System.FilePath ( (<.>), (</>) )
import System.IO
    ( hClose,
      hSeek,
      hPutStr,
      hGetLine,
      openFile,
      SeekMode(AbsoluteSeek, SeekFromEnd),
      IOMode(ReadWriteMode, WriteMode) )

import Extra.Helpers (compareTypes, splitOn, parseFields)

import AST

evalInsert tableName newData currentDatabase = do
    let tablePath = currentDatabase </> tableName <.> "txt"
    tableExists <- doesFileExist tablePath
    -- Revisar si existe la tabla
    if tableExists
        then do
            stream <- openFile tablePath ReadWriteMode
            fieldsAsStr <- hGetLine stream
            case parseFields fieldsAsStr of 
                Right fields -> do
                        --print fields
                        -- Revisar si el contenido es valido
                        isDataValid <- validateData fields newData
                        let dataToInsert = formatData newData
                        if isDataValid
                            then do
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
    case (fields !! index) of
        String _ len -> case strToInsert of
                            S _ l -> l >= len
                            _ -> False    
        _ -> False

-- Convertir a string la informacion a insertar teniendo en cuenta las reglas definidas
formatData [reg] = formatReg reg ++ "\n"
formatData (reg:regs) = formatReg reg ++ "\n" ++ formatData regs

--Ejemplo: Se formatea -> [S "Esteban",I 20,B True] a ("Esteban", String, 20)|(20, Integer)|(True, Bool)\n
formatReg [value] =
    "(" ++ getValue value ++ "," ++
    getDataType value ++
    resolveLength value
formatReg (value:values) =
    "(" ++ getValue value ++ "," ++
    getDataType value ++
    resolveLength value ++ "|" ++
    formatReg values

-- Funciones auxiliares de formatReg
getValue (S str _) = str
getValue (B True) = "true"
getValue (B False) = "false"
getValue (I num) = show num

getDataType (S _ _) = "string"
getDataType (B _) = "bool"
getDataType (I _) = "integer"

resolveLength (S str _) = "," ++ show (length str) ++ ")"
resolveLength _ = ")"
