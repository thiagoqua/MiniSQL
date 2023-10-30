module Evals.EvalSelect (evalSelect) where

import AST

import Extra.Helpers
import Evals.EvalCondition (verifCond')

import System.Directory ( doesFileExist, renameFile )
import System.FilePath ( (<.>), (</>) )
import System.IO
    ( hClose,
      hSeek,
      hSetFileSize,
      hGetLine,
      hPutStr,
      openFile,
      SeekMode(AbsoluteSeek),
      IOMode(ReadWriteMode, WriteMode), hGetContents, hPutStrLn )
import Extra.Printers (printAllColumns, printSelectedColumns)

evalSelect columns tableName cond clause currentDatabase = do
    let tablePath = currentDatabase </> tableName <.> "txt"
    tableExists <- doesFileExist tablePath
    -- Revisar que la tabla exista
    if tableExists
        then do
            stream <- openFile tablePath ReadWriteMode
            fields <- hGetLine stream
            -- Guardar todo el contenido del archivo, menos la primera linea
            contents <- hGetContents stream
            -- Dividir el contenido en líneas
            let registers = lines contents
            case columns of
                Asterisk -> do
                    -- evaluar si hay condicion
                    case cond of
                        CoSkip -> do
                            -- evaluar si hay clausula
                            case clause of
                                ClSkip -> printAllColumns fields registers
                                OrderBy name sort -> do
                                    if columnExists name fields
                                    then do
                                        index <- findColumnPosition name fields 0
                                        let orderRegs = qsort registers index sort
                                        printAllColumns fields orderRegs
                                    else do
                                        putStrLn $ "\nLa columna " ++ name ++ " no existe en la base de datos."
                        _ -> do
                                -- evaluar la condicion
                                case verifCond registers cond fields of
                                    Right registersToSelect -> 
                                        -- evaluar la clausula
                                        case clause of
                                            ClSkip -> printAllColumns fields registersToSelect
                                            OrderBy name sort -> do
                                                if columnExists name fields
                                                then do
                                                    index <- findColumnPosition name fields 0
                                                    let orderRegs = qsort registersToSelect index sort
                                                    printAllColumns fields orderRegs
                                                else do
                                                    putStrLn $ "\nLa columna " ++ name ++ " no existe en la base de datos."
                                    Left error -> putStrLn error
                Columns cols -> do
                    if checkColumnName cols fields
                        then do
                            case cond of
                                CoSkip -> do
                                    case clause of
                                        ClSkip -> do
                                            let indexes = findIndexes fields cols
                                            printSelectedColumns fields registers indexes
                                        OrderBy name sort -> do
                                            -- revisar los nombres de la columna/alias
                                            let columnName = checkNames name cols
                                            if columnExists columnName fields
                                            then do
                                                index <- findColumnPosition columnName fields 0
                                                let orderRegs = qsort registers index sort
                                                let indexes = findIndexes fields cols
                                                printSelectedColumns fields orderRegs indexes
                                            else do
                                                putStrLn $ "\nLa columna " ++ name ++ " no existe en la base de datos."
                                _ -> do
                                        -- revisar los nombres de la columna/alias
                                        let validCond = checkConditionNames cond cols
                                        -- evaluar la condicion
                                        case verifCond registers validCond fields of
                                            Right registersToSelect ->
                                                -- evaluar la clausula
                                                case clause of
                                                    ClSkip -> do
                                                        let indexes = findIndexes fields cols
                                                        printSelectedColumns fields registersToSelect indexes
                                                    OrderBy name sort -> do
                                                        -- revisar los nombres de la columna/alias
                                                        let columnName = checkNames name cols
                                                        if columnExists columnName fields
                                                        then do
                                                            index <- findColumnPosition columnName fields 0
                                                            let orderRegs = qsort registersToSelect index sort
                                                            let indexes = findIndexes fields cols
                                                            printSelectedColumns fields orderRegs indexes
                                                        else do
                                                            putStrLn $ "\nLa columna " ++ name ++ " no existe en la base de datos."
                                            Left error -> putStrLn error
                        else do
                            putStrLn "Hay una columna (o varias) que no existe en la base de datos."
            hClose stream
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
verifCond (x:xs) cond fields = 
    case verifCond' x cond fields of
        Right True -> (x :) <$> verifCond xs cond fields
        Right False -> verifCond xs cond fields
        Left errorMsg -> Left errorMsg

checkColumnName [] _ = True
checkColumnName ((col,_) : cols) fields = 
    if columnExists col fields
        then checkColumnName cols fields
        else False

-- Funciones para devolver la longitud del string en 'x' posición (index)
findValue reg = parseCampo reg 0        --reg -> "(2,integer)|(tiki,string,4)|(19,integer)|(false,bool)"

parseCampo str i =
    let elems = map (filter (`notElem` "()")) (splitOn ',' str)
    in elems !! i

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
    if getColumnValue reg idx < getColumnValue p idx
    then reg : menores regs p idx
    else menores regs p idx

mayores [] _ _ = []
mayores (reg:regs) p idx =
    if getColumnValue reg idx >= getColumnValue p idx
    then reg : mayores regs p idx
    else mayores regs p idx

getColumnValue reg i =
    let regToList = splitOn '|' reg
    in findValue (regToList !! i)
