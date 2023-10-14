module Evals.Eval (eval) where

import AST
import Evals.EvalSelect (evalSelect)
import Evals.EvalDelete (evalDelete)
import Evals.EvalCreate (evalTable, evalDatabase)
import Evals.EvalInsert (evalInsert)

-- Objeto que sirve para especificar en que base de datos se ejecutan las consultas/comandos
-- Se setea en "use" y lo utilizan todos los comandos
newtype EvalState = EvalState { currentDatabase :: Maybe String }

initState = EvalState { currentDatabase = Nothing }

-- Funcion que llama a los evaluadores de los comandos
eval = commEval initState

commEval state Skip = do putStrLn "Archivo ejecutado exitosamente."

commEval state (Seq Skip c2) = commEval state c2

commEval state (Seq c1 c2) = do commEval state c1
                                commEval state c2

commEval state (Use dbName c2) = do putStrLn $ "Cambiando a la base de datos '" ++ dbName ++ "'."
                                    let newState = state { currentDatabase = Just dbName }
                                    commEval newState c2

commEval state (CreateDatabase dbName c1) = do evalDatabase dbName
                                               commEval state c1

commEval state (CreateTable name columnCreation) = evalTable name columnCreation (currentDatabase state)

commEval state (Delete name cond) = evalDelete name cond (currentDatabase state)

commEval state (Insert name newData) = evalInsert name newData (currentDatabase state)

commEval state (Select columns from cond clause) = evalSelect columns from cond clause (currentDatabase state)
