module Evals.EvalCreate (evalDatabase, evalTable) where

import System.Directory
import System.FilePath
import AST

evalDatabase databaseName = do let databasePath = "./" ++ databaseName
                               createDirectory databasePath
                               putStrLn $ "Base de datos '" ++ databaseName ++ "' creada."


evalTable name columnCreation currentDatabase = do
    -- Revisar la BDD actual
    case currentDatabase of
        Just dbName -> do
            let tablePath = dbName </> name <.> "txt"
            tableExists <- doesFileExist tablePath
            -- Revisar que la tabla exista
            if tableExists
                then putStrLn $ "La tabla '" ++ name ++ "' ya existe en la base de datos '" ++ dbName ++ "'."
                else do
                    writeFile tablePath (tableDefinition columnCreation)
                    putStrLn $ "Tabla '" ++ name ++ "' creada en la base de datos '" ++ dbName ++ "'."
        Nothing -> do
            putStrLn "No se ha seleccionado una base de datos."

-- Funcion que genera un string de acuerdo a las reglas para la creacion de una tabla
tableDefinition [x] = struct x ++ "\n"
tableDefinition (x:xs) = struct x ++ "|" ++ tableDefinition xs

-- Funcion auxiliar de tableDefinition
struct x = "(" ++ name x ++ "," ++ dtype x ++ "," ++ long x ++ ")"
    where 
        name (Column colName _ _) = colName
        dtype (Column _ dtype _) = dtype
        long (Column _ _ long) = show long