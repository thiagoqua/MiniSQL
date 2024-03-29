module Evals.Eval (eval) where

import AST
    ( Command(Select, Skip, Seq, Use, CreateDatabase, CreateTable,
              Delete, Insert) )
import Evals.EvalSelect (evalSelect)
import Evals.EvalDelete (evalDelete)
import Evals.EvalCreate (evalTable, evalDatabase)
import Evals.EvalInsert (evalInsert)
import System.Directory ( doesDirectoryExist )
import System.FilePath ( (</>) )

-- Objeto que sirve para especificar en que base de datos se ejecutan las consultas/comandos
-- Se setea en "use" y lo utilizan todos los comandos
newtype EvalState = EvalState { currentDatabase :: String }

initState = EvalState { currentDatabase = "" }

-- Funcion que llama a los evaluadores de los comandos
eval = commEval initState

commEval state Skip = do putStrLn "Archivo ejecutado exitosamente."

commEval state (Seq Skip c2) = commEval state c2

commEval state (Seq c1 c2) = do commEval state c1
                                commEval state c2

commEval state (Use dbName c2) = do let databasePath = "./database" </> dbName
                                    dbExists <- doesDirectoryExist databasePath
                                    if dbExists
                                        then do putStrLn $ "Cambiando a la base de datos '" ++ dbName ++ "'."
                                                let newState = state { currentDatabase = databasePath }
                                                commEval newState c2
                                        else
                                            putStrLn "La base de datos que se quiere seleccionar no existe."

commEval state (CreateDatabase dbName c1) = do evalDatabase dbName
                                               commEval state c1

commEval state (CreateTable name columnCreation) = evalTable name columnCreation (currentDatabase state)

commEval state (Delete name cond) = evalDelete name cond (currentDatabase state)

commEval state (Insert name newData) = evalInsert name newData (currentDatabase state)

commEval state (Select columns from cond clause) = evalSelect columns from cond clause (currentDatabase state)
