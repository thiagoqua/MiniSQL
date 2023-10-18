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

import Extra.Helpers (compareTypes, splitOn)
import Evals.EvalCondition (verifCond')

import AST

evalInsert tableName newData currentDatabase = do
    -- Revisar la BDD actual
    case currentDatabase of
        Just dbName -> do
            let tablePath = dbName </> tableName <.> "txt"
            tableExists <- doesFileExist tablePath
            -- Revisar si existe la tabla
            if tableExists
                then do
                    stream <- openFile tablePath ReadWriteMode
                    fields <- hGetLine stream
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
                            putStrLn $ "Registro añadido exitosamente."
                        else hClose stream
                else do
                    putStrLn $ "La tabla '" ++ tableName ++ "' no existe en la base de datos '" ++ dbName ++ "'."
        Nothing -> do
            putStrLn "No se ha seleccionado una base de datos."

validateData _ [] = return True
validateData fields (reg:regs) = do
    let columns = splitOn '|' fields  -- ["id-integer", "name-string"]
    if length columns /= length reg
        then do
            putStrLn "La cantidad de datos a insertar es distinta a la cantidad de columnas de la tabla."
            return False
        else do
            isValidColumnType <- validateColumnTypes columns reg 0
            if isValidColumnType
                then validateData fields regs
                else return False

-- Validar si el tipo de las columnas coincide con los tipos de los datos a ingresar
validateColumnTypes fields reg index = do
    if index == length fields
        then 
            return True
        else do
            let colType = findDataType fields index                    
            if compareTypes (reg !! index) colType
                then
                    if colType == "string"
                        then do
                            isValidColumnLength <- validateColumnLength fields reg index
                            if isValidColumnLength
                                then
                                    validateColumnTypes fields reg (index + 1)
                                else do
                                    putStrLn "La longitud del string supera el limite establecido."
                                    return False
                        else
                            validateColumnTypes fields reg (index + 1)
                else do
                    putStrLn "Los tipos de datos no coinciden."
                    return False

-- Validar si la longitud del string definido en la columna coincide con la longitud del dato a ingresar
validateColumnLength fields reg index = do
    let validLength = findLength fields index
    let strToInsert = reg !! index
    return (compareLengths strToInsert validLength)

-- Comparar longitudes de strings
compareLengths (S str) validLength = 
    case readMaybe validLength of
        Just value -> length str <= value
        Nothing -> False

-- Devolver el tipo de dato del campo en 'x' posición (index)
findDataType campos idx = parseCampo (campos !! idx) 1

-- Devolver la longitud del string en 'x' posición (index)
findLength campos idx = parseCampo (campos !! idx) 2

parseCampo str i =
    let elems = map (filter (`notElem` "()")) (splitOn ',' str)
    in elems !! i


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
getValue (S str) = str
getValue (B True) = "true"
getValue (B False) = "false"
getValue (I num) = show num

getDataType (S _) = "string"
getDataType (B _) = "bool"
getDataType (I _) = "integer"

resolveLength (S str) = "," ++ show (length str) ++ ")"
resolveLength _ = ")"


