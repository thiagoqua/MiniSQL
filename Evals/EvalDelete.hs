{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Evals.EvalDelete (evalDelete) where
    
import AST (Cond (..), Op (Eq, Lt, Bt, Lte, Bte, Neq), PrimalType (S, B, I))
import System.Directory ( doesFileExist, renameFile )
import System.FilePath ( (<.>), (</>) )
import System.IO
    ( hClose,
      hSeek,
      hSetFileSize,
      hGetLine,
      hPutStr,
      openFile,
      SeekMode(AbsoluteSeek),
      IOMode(ReadWriteMode, WriteMode), hGetContents, hPutStrLn )
import Data.List (isInfixOf)
import Control.Monad
import Data.Char
import Text.Read (readMaybe)
import Data.Array (indices)

evalDelete name condition currentDatabase = do
    -- Revisar la BDD actual
    case currentDatabase of
        Just dbName -> do
            let tablePath = dbName </> name <.> "txt"
            tableExists <- doesFileExist tablePath
            -- Revisar que la tabla exista
            if tableExists
                then
                    case condition of
                        CoSkip -> do
                            -- Abrir archivo en modo de lectura/escritura
                            stream <- openFile tablePath ReadWriteMode
                            -- Leer la primera línea y guardarla en una variable
                            fields <- hGetLine stream
                            -- Posicionar el puntero del archivo al principio
                            hSeek stream AbsoluteSeek 0
                            -- Eliminar el contenido actual del archivo
                            hSetFileSize stream 0
                            -- Escribir la primera linea de vuelta en el archivo
                            hPutStr stream (fields ++ "\n")
                            -- Cerrar el archivo
                            hClose stream
                            putStrLn $ "Registros eliminados de la tabla '" ++ name ++ "' en la base de datos '" ++ dbName ++ "'."
                        condition -> do
                            stream <- openFile tablePath ReadWriteMode
                            fields <- hGetLine stream
                            -- Guardar todo el contenido del archivo, menos la primera linea
                            contents <- hGetContents stream
                            -- Dividir el contenido en líneas
                            let registers = lines contents
                            -- registersToInsert contiene los registros que NO cumplen con la condicion
                            -- Como no cumplen la condicion, se guardaran posteriormente en el archivo
                            let registersToInsert = verifCond registers condition fields
                            -- Crear un nuevo archivo temporal para escribir los registros actualizados
                            let tempTablePath = tablePath ++ ".temp"
                            -- Abrir el nuevo archivo en modo escritura
                            newStream <- openFile tempTablePath WriteMode
                            -- Escribir la primera línea (que contiene los campos) en el nuevo archivo
                            hPutStrLn newStream fields
                            -- Escribir los registros actualizados en el nuevo archivo
                            mapM_ (hPutStrLn newStream) registersToInsert
                            -- Cerrar el nuevo archivo
                            hClose newStream
                            -- Renombrar el archivo temporal para reemplazar el original
                            renameFile tempTablePath tablePath
                else do
                    putStrLn $ "La tabla '" ++ name ++ "' no existe en la base de datos '" ++ dbName ++ "'."
        Nothing -> do
                    putStrLn "No se ha seleccionado una base de datos."


-- Verifica que se cumpla la condicion en todos los registros
verifCond [] _ _ = []
verifCond (x:xs) cond fields = if verifCond' x cond fields
    then verifCond xs cond fields
    else x : verifCond xs cond fields

-- Funcion auxiliar para verificar que se cumpla la condicion en un registro
verifCond' reg (CAnd cond1 cond2) fields = verifCond' reg cond1 fields && verifCond' reg cond2 fields
verifCond' reg (COr cond1 cond2) fields = verifCond' reg cond1 fields || verifCond' reg cond2 fields
verifCond' reg (CNot cond) fields = not (verifCond' reg cond fields)
verifCond' reg (Exp op name primalType) fields = do
    if name `isInfixOf` fields
        then do
            -- Devolver tipo de dato de la columna especificada
            let colType = findDataType fields name
            case colType of
                Just typeStr ->
                    -- Comparar el tipo de dato de la columna con el tipo de dato del valor a comparar
                    if compareTypes primalType typeStr
                        then
                            let value = splitColumns reg name fields
                            in evalCond op value primalType
                            --putStrLn $ "Registros eliminados de la tabla '" ++ name ++ "' en la base de datos '" ++ dbName ++ "'."
                        else False
                             --putStrLn "El tipo de dato del valor a comparar no coincide con el tipo de dato del campo."
                Nothing -> False
                           --putStrLn "No se encontró el tipo de dato de la columna.";
        else False
            --putStrLn $ "No se encontró el nombre de la columna '" ++ name ++ "' en 'fields'."

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

-- Función para comparar tipos de datos
compareTypes :: PrimalType -> String -> Bool
compareTypes (S _) "string" = True
compareTypes (I _) "integer" = True
compareTypes (B _) "bool" = True
compareTypes _ _ = False

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


splitOn _ [] = []
splitOn delimiter list = let (first, rest) = break (== delimiter) list
                         in first : case rest of
                             [] -> []
                             (_:xs) -> splitOn delimiter xs