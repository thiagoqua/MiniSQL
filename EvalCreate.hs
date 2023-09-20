module EvalCreate (evalDatabase, evalTable) where

import System.Directory
import System.FilePath
import AST

evalDatabase databaseName = do let databasePath = "./" ++ databaseName
                               createDirectory databasePath
                               putStrLn $ "Base de datos '" ++ databaseName ++ "' creada."


evalTable name columnCreation currentDatabase = do
    case currentDatabase of
        Just dbName -> do
            let tablePath = dbName </> name <.> "txt"
            tableExists <- doesFileExist tablePath
            if tableExists
                then putStrLn $ "La tabla '" ++ name ++ "' ya existe en la base de datos '" ++ dbName ++ "'."
                else do
                    writeFile tablePath (tableDefinition columnCreation)
                    putStrLn $ "Tabla '" ++ name ++ "' creada en la base de datos '" ++ dbName ++ "'."
        Nothing -> do
            putStrLn "No se ha seleccionado una base de datos."

tableDefinition [] = []
tableDefinition [x] = struct x ++ "\n"
tableDefinition (x:xs) = struct x ++ "|" ++ tableDefinition xs

struct x = "(" ++ name x ++ "," ++ dtype x ++ "," ++ long x ++ ")"
name (Column colName _ _) = colName
dtype (Column _ dtype _) = dtype
long (Column _ _ long) = show long            