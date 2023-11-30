module Evals.EvalCreate (evalDatabase, evalTable) where

import System.Directory (createDirectory, doesDirectoryExist, doesFileExist)
import System.FilePath ( (<.>), (</>), takeFileName )
import AST ( ColumnCreation(Column), Field(Bool, String, Integer) )
import Data.List (nub)
import Control.Monad (when)
import Extra.Helpers (validateDirectories)

-- Librerías para cambiar los permisos de los archivos de tablas

-- Linux
--import System.Posix.Files
--import System.Posix.Types

evalDatabase databaseName = do 
    dbDirExists <- doesDirectoryExist "database"
    when (not dbDirExists) $ 
        createDirectory "database"
    let databasePath = "database" </> databaseName
    directoryExists <- doesDirectoryExist databasePath
    if directoryExists
        then putStrLn $ "La base de datos '" ++ databaseName ++ "' que intenta crear ya existe."
        else do createDirectory databasePath
                putStrLn $ "Base de datos '" ++ databaseName ++ "' creada."

evalTable name columnCreation currentDatabase = do
    (tableExists, dbName, tablePath) <- validateDirectories name currentDatabase
    -- Revisar que la tabla exista
    if tableExists
        then putStrLn $ "La tabla '" ++ name ++ "' ya existe en la base de datos '" ++ dbName ++ "'."
        else do
            valid <- evalColumnNames name columnCreation
            when valid $ do
                    writeFile tablePath (tableDefinition columnCreation)
                    -- setTablePermissions tablePath
                    putStrLn $ "Tabla '" ++ name ++ "' creada en la base de datos '" ++ dbName ++ "'."

evalColumnNames tableName columns = do
    let list = extractName columns
    noDuplicate <- hasNoColumnDuplicate tableName list
    noEqTableName <- hasNoEqualTableName tableName columns
    return (noDuplicate && noEqTableName)

extractName [] = []
extractName ((Column name _) : xs) = name : extractName xs

-- Verifica que los nombres de las columnas de una tabla no sean iguales
hasNoColumnDuplicate name xs = do
    if length xs == length (nub xs)
    then return True
    else do
        putStrLn $ "La tabla '" ++ name ++ "' tiene columnas repetidas."
        return False

-- Verifica que los nombres de las columnas de la tabla no sean iguales al de la tabla
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
struct x = "(" ++ name x ++ "," ++ value x  ++ ")"
    where
        name (Column colName _) = colName
        value (Column _ dtype) = 
            case dtype of
                String _ len -> "string" ++ "," ++ show len
                Integer _ -> "integer"
                Bool _ -> "bool"

--setTablePermissions tablePath = do
--    let readOnlyMode = ownerReadMode `unionFileModes` groupReadMode `unionFileModes` otherReadMode
--    setFileMode tablePath readOnlyMode
