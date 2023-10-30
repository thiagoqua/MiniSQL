module Evals.EvalCreate (evalDatabase, evalTable) where

import System.Directory (createDirectory, doesDirectoryExist, doesFileExist)
import System.FilePath
import AST

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
            writeFile tablePath (tableDefinition columnCreation)
            -- setTablePermissions tablePath
            putStrLn $ "Tabla '" ++ name ++ "' creada en la base de datos '" ++ currentDatabase ++ "'."

-- Funcion que genera un string de acuerdo a las reglas para la creacion de una tabla
tableDefinition [x] = struct x ++ "\n"
tableDefinition (x:xs) = struct x ++ "|" ++ tableDefinition xs

-- Funcion auxiliar de tableDefinition
struct x = "(" ++ name x ++ "," ++ dtype x ++ "," ++ long x ++ ")"
    where 
        name (Column colName _ _) = colName
        dtype (Column _ dtype _) = dtype
        long (Column _ _ long) = show long

--setTablePermissions tablePath = do
--    let readOnlyMode = ownerReadMode `unionFileModes` groupReadMode `unionFileModes` otherReadMode
--    setFileMode tablePath readOnlyMode
