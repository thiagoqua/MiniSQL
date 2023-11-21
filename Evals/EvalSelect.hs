module Evals.EvalSelect (evalSelect) where

import AST
    ( As(As, ASkip),
      Clause(OrderBy, ClSkip),
      ColumnName,
      Columns(Columns, Asterisk),
      Cond(Exp, CoSkip, CAnd, COr, CNot),
      Field,
      PrimalType(S, I, B),
      Sort(DESC, ASC) )

import Extra.Helpers
    ( columnExists,
      findColumnPosition,
      findIndexes,
      parseContent,
      parseFields, openTable )
import Evals.EvalCondition (verifCond')

import System.Directory ( doesFileExist )
import System.FilePath ( (<.>), (</>) )
import System.IO
    ( hClose, hGetContents )
import Extra.Printers (printAllColumns, printSelectedColumns)

evalSelect columns tableName cond clause currentDatabase = do
    let tablePath = currentDatabase </> tableName <.> "txt"
    tableExists <- doesFileExist tablePath
    -- Revisar que la tabla exista
    if tableExists
        then do
            (stream,fieldsAsStr) <- openTable tablePath
            case parseFields fieldsAsStr of
                Right fields -> do
                    -- Guardar todo el contenido del archivo, menos la primera linea
                    contentsAsStr <- hGetContents stream
                    case parseContent contentsAsStr of
                        Right [] -> putStrLn $ "No hay registros en la tabla '" ++ tableName ++ "' para seleccionar."
                        Right contents -> do
                            case columns of
                                Asterisk -> do
                                    -- evaluar si hay condicion
                                    case cond of
                                        CoSkip -> do
                                            -- evaluar si hay clausula
                                            case clause of
                                                ClSkip -> printAllColumns fields contents tableName
                                                OrderBy name sort -> do
                                                    if columnExists name fields
                                                    then do
                                                        index <- findColumnPosition name fields 0
                                                        let orderRegs = qsort contents index sort
                                                        printAllColumns fields orderRegs tableName
                                                    else do
                                                        putStrLn $ "\nLa columna '" ++ name ++ "' no existe en la tabla."
                                        _ -> do
                                                -- evaluar la condicion
                                                case verifCond contents cond fields of
                                                    Right [] -> putStrLn $ "No hay registros que cumplan la condición en la tabla '" ++ tableName ++ "'."
                                                    Right registersToSelect ->
                                                        -- evaluar la clausula
                                                        case clause of
                                                            ClSkip -> printAllColumns fields registersToSelect tableName
                                                            OrderBy name sort -> do
                                                                if columnExists name fields
                                                                then do
                                                                    index <- findColumnPosition name fields 0
                                                                    let orderRegs = qsort registersToSelect index sort
                                                                    printAllColumns fields orderRegs tableName
                                                                else do
                                                                    putStrLn $ "\nLa columna '" ++ name ++ "' no existe en la tabla."
                                                    Left error -> putStrLn error
                                Columns cols -> do
                                    if checkColumnName cols fields
                                        then do
                                            case cond of
                                                CoSkip -> do
                                                    case clause of
                                                        ClSkip -> do
                                                            let indexes = findIndexes fields cols
                                                            printSelectedColumns fields contents indexes tableName
                                                        OrderBy name sort -> do
                                                            -- revisar los nombres de la columna/alias
                                                            let columnName = checkNames name cols
                                                            if columnExists columnName fields
                                                            then do
                                                                index <- findColumnPosition columnName fields 0
                                                                let orderRegs = qsort contents index sort
                                                                let indexes = findIndexes fields cols
                                                                printSelectedColumns fields orderRegs indexes tableName
                                                            else do
                                                                putStrLn $ "\nLa columna '" ++ name ++ "' no existe en la tabla."
                                                _ -> do
                                                        -- revisar los nombres de la columna/alias
                                                        let validCond = checkConditionNames cond cols
                                                        -- evaluar la condicion
                                                        case verifCond contents validCond fields of
                                                            Right [] -> putStrLn $ "No hay registros que cumplan la condición en la tabla '" ++ tableName ++ "'."
                                                            Right registersToSelect ->
                                                                -- evaluar la clausula
                                                                case clause of
                                                                    ClSkip -> do
                                                                        let indexes = findIndexes fields cols
                                                                        printSelectedColumns fields registersToSelect indexes tableName
                                                                    OrderBy name sort -> do
                                                                        -- revisar los nombres de la columna/alias
                                                                        let columnName = checkNames name cols
                                                                        if columnExists columnName fields
                                                                        then do
                                                                            index <- findColumnPosition columnName fields 0
                                                                            let orderRegs = qsort registersToSelect index sort
                                                                            let indexes = findIndexes fields cols
                                                                            printSelectedColumns fields orderRegs indexes tableName
                                                                        else do
                                                                            putStrLn $ "\nLa columna '" ++ name ++ "' no existe en la tabla."
                                                            Left error -> putStrLn error
                                        else do
                                            putStrLn "Hay una columna (o varias) que no existe en la base de datos."
                            hClose stream
                        Left err -> do putStrLn "Hubo un error interno:\n"
                                       print err
                Left err -> do putStrLn "Hubo un error interno:\n"
                               print err
        else putStrLn $ "La tabla '" ++ tableName ++ "' no existe en la base de datos."

-- chequea que el alias corresponda a un nombre de columna
-- si no es un alias, entonces se trata del nombre de la columna
-- si no es ninguno de los dos, de eso se encarga verifCond (tira una excepcion)
checkNames name [] = name
checkNames name ((_,ASkip):cols) = checkNames name cols
checkNames name ((column,As alias):cols) =
    if name == alias
        then column
        else checkNames name cols

 -- devuelve una nueva condición con los alias cambiados por los correspondientes
 -- nombres de columnas
checkConditionNames CoSkip _ = CoSkip
checkConditionNames (CAnd s1 s2) cols = CAnd (checkConditionNames s1 cols) (checkConditionNames s2 cols)
checkConditionNames (COr s1 s2) cols = COr (checkConditionNames s1 cols) (checkConditionNames s2 cols)
checkConditionNames (CNot s1) cols = CNot (checkConditionNames s1 cols)
checkConditionNames (Exp op name primalType) cols = Exp op columnName primalType
    where
        columnName = checkNames name cols

verifCond [] _ _ = Right []
verifCond (x:xs) cond fields = do
    case verifCond' x cond fields of
        Right True -> (x :) <$> verifCond xs cond fields
        Right False -> verifCond xs cond fields
        Left errorMsg -> Left errorMsg

checkColumnName :: [ColumnName] -> [Field] -> Bool
checkColumnName [] _ = True
checkColumnName ((col,_) : cols) fields =
    columnExists col fields && checkColumnName cols fields

-- quicksort y funciones auxiliares para ordenar por order by
qsort [] _ _ = []
qsort [x] _ _ = [x]
qsort (reg:regs) idx sort = left ++ [reg] ++ right
    where
        left = case sort of
            ASC -> qsort men idx sort
            DESC -> qsort may idx sort
        right = case sort of
            ASC -> qsort may idx sort
            DESC -> qsort men idx sort
        men = menores regs reg idx
        may = mayores regs reg idx

menores [] _ _ = []
menores (reg:regs) p idx =
    let (rv,pv) = getPivoteAndRegValues reg p idx
    in  if lessThan rv pv
        then reg : menores regs p idx
        else menores regs p idx

mayores [] _ _ = []
mayores (reg:regs) p idx =
    let (rv,pv) = getPivoteAndRegValues reg p idx
    in  if not (lessThan rv pv)
        then reg : mayores regs p idx
        else mayores regs p idx

lessThan (I valueReg) (I valuePiv) = valueReg < valuePiv
lessThan (B valueReg) (B valuePiv) = valueReg < valuePiv
lessThan (S valueReg _) (S valuePiv _) = valueReg < valuePiv

getPivoteAndRegValues reg piv idx = (rValue,pValue)
    where
        rValue = reg !! idx
        pValue = piv !! idx
