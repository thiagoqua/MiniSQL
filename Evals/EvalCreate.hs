module Evals.EvalCreate (evalDatabase, evalTable) where

import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath
import AST
import Data.List (nub)
import Control.Monad (when)
-- Librerías para cambiar los permisos de los archivos de tablas

-- Linux
--import System.Posix.Files
--import System.Posix.Types

evalDatabase databaseName = do let databasePath = "./" ++ databaseName
                               directoryExists <- doesDirectoryExist databasePath
                               if directoryExists
                                then putStrLn $ "La base de datos '" ++ databaseName ++ "' que intenta crear ya existe."
                                else do System.Directory.createDirectory databasePath
                                        putStrLn $ "Base de datos '" ++ databaseName ++ "' creada."

evalTable name columnCreation currentDatabase = do
    let tablePath = currentDatabase </> name <.> "txt"
    tableExists <- doesFileExist tablePath
    -- Revisar que la tabla exista
    if tableExists
        then putStrLn $ "La tabla '" ++ name ++ "' ya existe en la base de datos '" ++ currentDatabase ++ "'."
        else do
            valid <- evalColumnNames name columnCreation
            when valid $ do
                    writeFile tablePath (tableDefinition columnCreation)
                    -- setTablePermissions tablePath
                    putStrLn $ "Tabla '" ++ name ++ "' creada en la base de datos '" ++ currentDatabase ++ "'."

evalColumnNames tableName columns = do
    let list = extractName columns
    noDuplicate <- hasNoDuplicates tableName list
    noEqTableName <- hasNoEqualTableName tableName columns
    return (noDuplicate && noEqTableName)

-- Funciones para verificar que los nombres de las columnas de una tabla no sean iguales
extractName [] = []
extractName ((Column name _) : xs) = name : extractName xs

hasNoDuplicates name xs = do
    if length xs == length (nub xs)
    then return True
    else do
        putStrLn $ "La tabla '" ++ name ++ "' tiene columnas repetidas."
        return False

-- Funcion que verifica que las columnas de la tabla no tengan nombres iguales a la tabla
hasNoEqualTableName _ [] = return True
hasNoEqualTableName tableName ((Column name _) : xs) =
    if tableName == name
    then do
        putStrLn $ "El nombre de alguna columna coincide con el de la tabla '" ++ tableName ++ "'."
        return False
    else hasNoEqualTableName tableName xs

-- Funcion que genera un string de acuerdo a las reglas para la creacion de una tabla
tableDefinition [x] = struct x ++ "\n"
tableDefinition (x:xs) = struct x ++ "|" ++ tableDefinition xs

-- Funcion auxiliar de tableDefinition
struct x = "(" ++ name x ++ "," ++ fst (dtype x) ++ "," ++ snd (dtype x) ++ ")"
    where
        name (Column colName _) = colName
        dtype (Column _ dtype) = 
            case dtype of
                String _ len -> ("string",show len)
                Integer _ -> ("integer","0")
                Bool _ -> ("bool","0")

--setTablePermissions tablePath = do
--    let readOnlyMode = ownerReadMode `unionFileModes` groupReadMode `unionFileModes` otherReadMode
--    setFileMode tablePath readOnlyMode
