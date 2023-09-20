{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module EvalDelete (evalDelete) where
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
    case currentDatabase of
        Just dbName -> do
            let tablePath = dbName </> name <.> "txt"
            tableExists <- doesFileExist tablePath
            if tableExists
                then
                    case condition of
                        CoSkip -> do
                            -- Abrir  archivo en modo de lectura/escritura.
                            stream <- openFile tablePath ReadWriteMode
                            -- Leer la primera línea y guardarla en una variable.
                            fields <- hGetLine stream
                            -- Posicionar el puntero del archivo al principio.
                            hSeek stream AbsoluteSeek 0
                            -- Eliminar el contenido actual del archivo.
                            hSetFileSize stream 0
                            -- Escribir el contenido modificado (la primera linea) de vuelta en el archivo.
                            hPutStr stream (fields ++ "\n")
                            -- Cerrar el archivo.
                            hClose stream
                            putStrLn $ "Registros eliminados de la tabla '" ++ name ++ "' en la base de datos '" ++ dbName ++ "'."
                        condition -> do
                            stream <- openFile tablePath ReadWriteMode
                            fields <- hGetLine stream   -- primera linea
                            contents <- hGetContents stream -- todo el archivo
                            let registers = lines contents -- Divide el contenido en líneas
                            let registersToInsert = verifCond registers condition fields -- registersToInsert contiene los registros que NO cumplen con la condicion
                               
                            -- Crear un nuevo archivo temporal para escribir los registros actualizados.
                            
                            let tempTablePath = tablePath ++ ".temp"
                            -- Abrir el nuevo archivo en modo escritura.
                            newStream <- openFile tempTablePath WriteMode
                            -- Escribir la primera línea de campos en el nuevo archivo.
                            hPutStrLn newStream fields
                            -- Escribir los registros actualizados en el nuevo archivo.
                            mapM_ (hPutStrLn newStream) registersToInsert
                            -- Cerrar el nuevo archivo.
                            hClose newStream

                            -- Renombrar el archivo temporal para reemplazar el original.
                            renameFile tempTablePath tablePath
                            
                            print registersToInsert
                else do
                    putStrLn $ "La tabla '" ++ name ++ "' no existe en la base de datos '" ++ dbName ++ "'."
        Nothing -> do
                    putStrLn "No se ha seleccionado una base de datos."


verifCond [] _ _ = []
verifCond (x:xs) cond fields = if verifCond' x cond fields
    then verifCond xs cond fields
    else x : verifCond xs cond fields

verifCond' reg (CAnd cond1 cond2) fields = verifCond' reg cond1 fields && verifCond' reg cond2 fields
verifCond' reg (COr cond1 cond2) fields = verifCond' reg cond1 fields || verifCond' reg cond2 fields
verifCond' reg (CNot cond) fields = not (verifCond' reg cond fields)
verifCond' reg (Exp op name primalType) fields = do
    if name `isInfixOf` fields
        then do
            -- devolver tipo de dato de la columna especificada
            let colType = buscarTipoDato fields name
            case colType of
                Just typeStr ->
                    -- comparamos tipo de dato de la columna con tipo de dato del valor a comparar
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

-- Función para comparar tipos de datos.
compareTypes :: PrimalType -> String -> Bool
compareTypes (S _) "string" = True
compareTypes (I _) "integer" = True
compareTypes (B _) "bool" = True
compareTypes _ _ = False

-- Funciones para encontrar el tipo de dato de una columna en la primera línea del archivo.

parseCampo str =
    let elems = map (filter (`notElem` "()")) (splitOn ',' str) -- saca parentesis, divide por coma y mapea cada string
    in (head elems, elems !! 1) -- 1 representa la posicion del tipo de dato

buscarTipoDato str campo =
    let campos = map parseCampo (splitOn '|' str) -- divide por | y mapea cada tupla
    in lookup campo campos


-- Funciones para encontrar el valor de una columna

splitColumns reg name fields = do 
    let columnsValues = splitOn '|' reg       --["(12,integer,)","(18,integer,)"]
    let columnsNames = splitOn '|' fields     --["(id,integer,)","(edad,integer,)"]
    let columnIndex = matchColumnName columnsNames name 0       --
    parseValor (columnsValues !! columnIndex)                        --"(18,integer,)"

parseValor column =
    let elems = map (filter (`notElem` "()")) (splitOn ',' column) -- saca parentesis, divide por coma y mapea cada string
    in head elems

matchColumnName (x:xs) name index = 
    if name `isInfixOf` x
        then index
        else matchColumnName xs name (index + 1)

splitOn _ [] = []
splitOn delimiter list =
    let (first, rest) = break (== delimiter) list
    in first : case rest of
        [] -> []
        (_:xs) -> splitOn delimiter xs