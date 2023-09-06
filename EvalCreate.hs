module EvalCreate (evalDatabase) where

import System.Directory

evalDatabase databaseName = do let databasePath = "./" ++ databaseName
                               createDirectory databasePath
                               putStrLn $ "Base de datos '" ++ databaseName ++ "' creada."