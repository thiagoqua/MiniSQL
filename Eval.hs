module Eval (eval) where

import AST
import EvalSelect
import EvalDelete
import EvalCreate
import EvalInsert

-- este objeto sirve para USAR la BDD (para especificar en que tablas se ejecutan las consultas/comandos), no para el CreateDatabase
newtype EvalState = EvalState { currentDatabase :: Maybe String }

initState = EvalState { currentDatabase = Nothing }

eval = comm initState

comm state Skip = do
    putStrLn "Archivo ejecutado exitosamente."
comm state (Seq Skip c2) = comm state c2
comm state (Seq c1 c2) = comm state c1

comm state (Use dbName c2) = do
    putStrLn $ "Cambiando a la base de datos '" ++ dbName ++ "'."
    let newState = state { currentDatabase = Just dbName }
    comm newState c2

comm state (CreateDatabase dbName c1) = do
    putStrLn $ "Creando la base de datos '" ++ dbName ++ "'."
    evalDatabase dbName
    comm state c1


--eval (Select columns from cond clause) s = evalSelect columns from cond clause
--eval (CreateTable name [columnCreation]) s = evalTable name [columnCreation]
--eval (Insert name [heterList]) s = evalInsert name [heterList]
--eval (Delete name cond) s = evalDelete name cond

