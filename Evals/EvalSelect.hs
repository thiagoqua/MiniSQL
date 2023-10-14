module Evals.EvalSelect (evalSelect) where

import AST

import Evals.Helpers
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
import Data.List (isInfixOf)
import Evals.Printers (printAllColumns, printSelectedColumns)

evalSelect columns tableName cond clause currentDatabase = do
    -- Revisar la BDD actual
    case currentDatabase of
        Just dbName -> do
            let tablePath = dbName </> tableName <.> "txt"
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
                            case cond of
                                CoSkip -> do
                                    case clause of
                                        ClSkip -> printAllColumns fields registers
                                        OrderBy name sort -> do
                                            if name `isInfixOf` fields
                                            then do
                                                index <- findColumnPosition name fields 0
                                                let orderRegs = orderBy sort registers index
                                                printAllColumns fields orderRegs
                                            else do
                                                putStrLn $ "La columna " ++ name ++ " no existe en la base de datos."
                                _ -> do
                                        let registersToSelect = verifCond registers cond fields
                                        case clause of
                                            ClSkip -> printAllColumns fields registersToSelect
                                            OrderBy name sort -> do
                                                if name `isInfixOf` fields
                                                then do
                                                    index <- findColumnPosition name fields 0
                                                    let orderRegs = orderBy sort registersToSelect index
                                                    printAllColumns fields orderRegs
                                                else do
                                                    putStrLn $ "La columna " ++ name ++ " no existe en la base de datos."

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
                                                    if name `isInfixOf` fields
                                                    then do
                                                        index <- findColumnPosition name fields 0
                                                        let orderRegs = orderBy sort registers index
                                                        let indexes = findIndexes fields cols
                                                        printSelectedColumns fields orderRegs indexes
                                                    else do
                                                        putStrLn $ "La columna " ++ name ++ " no existe en la base de datos."
                                        _ -> do
                                                let registersToSelect = verifCond registers cond fields
                                                case clause of
                                                    ClSkip -> do
                                                        let indexes = findIndexes fields cols
                                                        printSelectedColumns fields registersToSelect indexes
                                                    OrderBy name sort -> do
                                                        if name `isInfixOf` fields
                                                        then do
                                                            index <- findColumnPosition name fields 0
                                                            let orderRegs = orderBy sort registersToSelect index
                                                            let indexes = findIndexes fields cols
                                                            printSelectedColumns fields orderRegs indexes
                                                        else do
                                                            putStrLn $ "La columna " ++ name ++ " no existe en la base de datos."
                                else do
                                    putStrLn "Hay una columna (o varias) que no existe en la base de datos."
                    hClose stream
                else putStrLn "No existe la tabla seleccionada."

verifCond [] _ _ = []
verifCond (x:xs) cond fields = if verifCond' x cond fields
    then x : verifCond xs cond fields
    else verifCond xs cond fields

checkColumnName [] _ = True
checkColumnName ((col,_) : cols) fields = if col `isInfixOf` fields
                                        then checkColumnName cols fields
                                        else False

orderBy sort regs i = qsort regs i sort

-- Devolver la longitud del string en 'x' posición (index)
findValue reg = parseCampo reg 0        --reg -> "(2,integer)|(tiki,string,4)|(19,integer)|(false,bool)"

parseCampo str i =
    let elems = map (filter (`notElem` "()")) (splitOn ',' str)
    in elems !! i

-- quicksort para ordenar por ord by
qsort [] _ _ = []
qsort [x] _ _ = [x]
qsort (reg:regs) idx sort =
    left ++ [reg] ++ right
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
    if getColumnValue reg idx > getColumnValue p idx
    then reg : mayores regs p idx
    else mayores regs p idx

getColumnValue reg i =
    let regToList = splitOn '|' reg
    in findValue (regToList !! i)

-- FALTA ALIAS
