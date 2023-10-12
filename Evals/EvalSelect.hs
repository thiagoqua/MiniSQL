module Evals.EvalSelect (evalSelect) where

import AST (Cond (..),
    Op (Eq, Lt, Bt, Lte, Bte, Neq),
    PrimalType (S, B, I),
    Columns (Asterisk, Columns),
    Clause (ClSkip, OrderBy), Sort (ASC, DESC))

import Evals.Helpers (compareTypes, splitOn)
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
                                        ClSkip -> printSelected registers ([] :: [Int])
                                        OrderBy name sort -> do
                                            if name `isInfixOf` fields
                                            then do
                                                index <- findColumnPosition name fields 0
                                                let orderRegs = orderBy sort registers index
                                                printSelected orderRegs ([] :: [Int])
                                            else do
                                                putStrLn $ "La columna " ++ name ++ " no existe en la base de datos."
                                _ -> do
                                        let registersToSelect = verifCond registers cond fields
                                        case clause of
                                            ClSkip -> printSelected registersToSelect ([] :: [Int])
                                            OrderBy name sort -> do
                                                if name `isInfixOf` fields
                                                then do
                                                    index <- findColumnPosition name fields 0
                                                    let orderRegs = orderBy sort registersToSelect index
                                                    printSelected orderRegs ([] :: [Int])
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
                                                    printFields fields indexes
                                                    printSelected registers indexes
                                                OrderBy name sort -> do
                                                    if name `isInfixOf` fields
                                                    then do
                                                        index <- findColumnPosition name fields 0
                                                        let orderRegs = orderBy sort registers index
                                                        let indexes = findIndexes fields cols
                                                        printFields fields indexes
                                                        printSelected orderRegs indexes
                                                    else do
                                                        putStrLn $ "La columna " ++ name ++ " no existe en la base de datos."
                                        _ -> do
                                                let registersToSelect = verifCond registers cond fields
                                                case clause of
                                                    ClSkip -> do
                                                        let indexes = findIndexes fields cols
                                                        printFields fields indexes
                                                        printSelected registersToSelect indexes
                                                    OrderBy name sort -> do
                                                        if name `isInfixOf` fields
                                                        then do
                                                            index <- findColumnPosition name fields 0
                                                            let orderRegs = orderBy sort registersToSelect index
                                                            let indexes = findIndexes fields cols
                                                            printFields fields indexes
                                                            printSelected orderRegs indexes
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


findColumnPosition name fields idx = do
    if length fields == idx
        then return (-1)  -- no es necesario
        else do
            let columns = splitOn '|' fields    -- ["id-integer", "name-string"]
            if name `isInfixOf` (columns !! idx)
                then return idx
                else findColumnPosition name fields (idx+1)


-- QSORT
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

-- [(2,integer)|(tiki,string,4)|(19,integer) , (1,integer)|(este,string,4)|(18,integer)]"

-- obtenemos los indices de la posicion del nombre de las columnas (de la primera linea)
findIndexes _ [] = []
findIndexes fields ((col,_):cols) = do
    idx <- findColumnPosition col fields 0
    idx : findIndexes fields cols

printFields fields cols =  do
    let columns = splitOn '|' fields
    let regAsStr = makeRegString columns cols
    putStrLn regAsStr

printSelected regs [] = print regs
printSelected [reg] cols = do
    let columns = splitOn '|' reg
    let regAsStr = makeRegString columns cols
    putStrLn regAsStr
printSelected (reg:regs) cols = do
    let columns = splitOn '|' reg
    let regAsStr = makeRegString columns cols
    putStrLn regAsStr
    printSelected regs cols

-- cols - [0,1]
makeRegString reg [col] = reg !! col
makeRegString reg (col:cols) = (reg !! col) ++ makeRegString reg cols


-- FALTA ALIAS
-- FALTA IMPRIMIR BIEN LO DE ASTERISK (DEVUELVE UN ARREGLO CON LOS RESULTADOS)