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

import AST

evalInsert tableName newData currentDatabase = do
    -- Revisar la BDD actual
    case currentDatabase of
        Just dbName -> do
            let tablePath = dbName </> tableName <.> "txt"
            tableExists <- doesFileExist tablePath
            if tableExists
                then do
                    stream <- openFile tablePath ReadWriteMode
                    fields <- hGetLine stream
                    isDataValid <- validateData fields newData
                    let dataToInsert = formatData newData
                    if isDataValid
                        then do
                            hSeek stream SeekFromEnd 0
                            hPutStr stream dataToInsert
                            --print dataToInsert
                            hClose stream
                            putStrLn $ "Registro añadido exitosamente"
                        else hClose stream
                else do
                    putStrLn $ "La tabla '" ++ tableName ++ "' no existe en la base de datos '" ++ dbName ++ "'."
        Nothing -> do
            putStrLn "No se ha seleccionado una base de datos."

formatData [reg] = formatReg reg ++ "\n"
formatData (reg:regs) = formatReg reg ++ "\n" ++ formatData regs

--[S "Esteban",I 20,B True]
--("Esteban", String,x)|(20, Integer)|(True, Bool)\n
formatReg [value] = 
    "(" ++ getValue value ++ "," ++ 
    getDataType value ++ 
    resolveLength value
formatReg (value:values) = 
    "(" ++ getValue value ++ "," ++ 
    getDataType value ++ 
    resolveLength value ++ "|" ++
    formatReg values

getValue (S str) = str
getValue (B True) = "true"
getValue (B False) = "false"
getValue (I num) = show num

getDataType (S _) = "string"
getDataType (B _) = "bool"
getDataType (I _) = "integer"

resolveLength (S str) = "," ++ show (length str) ++ ")"
resolveLength _ = ")"

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

validateColumnLength fields reg index = do
    let validLength = findLength fields index
    let strToInsert = reg !! index
    return (compareLengths strToInsert validLength)

compareLengths (S str) validLength = 
    case readMaybe validLength of
        Just value -> length str <= value
        Nothing -> False

-- devuelve el tipo de dato del field en la posición index
findDataType campos idx = parseCampo (campos !! idx) 1

-- devuelve la longitud del string en la posición index
findLength campos idx = parseCampo (campos !! idx) 2

-- CUIDADO: si el tipo de dato no es string, y si i = 2, falla
parseCampo str i =
    let elems = map (filter (`notElem` "()")) (splitOn ',' str)
    in elems !! i

-- Funciones auxiliares (poner estas funciones en archivo aparte)
splitOn _ [] = []
splitOn delimiter list = let (first, rest) = break (== delimiter) list
                         in first : case rest of
                             [] -> []
                             (_:xs) -> splitOn delimiter xs
                             
-- Función para comparar tipos de datos
compareTypes :: PrimalType -> String -> Bool
compareTypes (S _) "string" = True
compareTypes (I _) "integer" = True
compareTypes (B _) "bool" = True
compareTypes _ _ = False