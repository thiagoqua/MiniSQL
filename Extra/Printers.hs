{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Extra.Printers (printAllColumns,printSelectedColumns) where

import Extra.Helpers ( extractColumnName, findIndexes )
import AST ( As(ASkip), Field, PrimalType(..) )

-- FUNCIONES PARA IMPRESIÓN POR PANTALLA

-- imprime todos los registros (asterisco)
printAllColumns::[Field] -> [[PrimalType]] -> String -> IO ()
printAllColumns fields regs tableName = do
    putStrLn $ "\nResultados del comando SELECT para la tabla '" ++ tableName ++ "':\n"
    let colNames = findAllColumnNames fields
    let cols = findIndexes fields colNames
    printColumns fields regs cols

-- busca y devuelve en tuplas todos los nombres de las columnas de la tabla
findAllColumnNames [] = []
findAllColumnNames (f:fields) = (colName,ASkip) : findAllColumnNames fields
    where
        colName = extractColumnName f

-- imprime el mensaje y llama a la siguiente
printSelectedColumns::[Field] -> [[PrimalType]] -> [Int] -> String -> IO ()
printSelectedColumns fields regs cols tableName = do
    putStrLn $ "\nResultados del comando SELECT para la tabla '" ++ tableName ++ "':\n"
    printColumns fields regs cols

-- busca el ancho máximo de las columnas y llama a la siguiente
printColumns fields regs cols = do
    let colsWidth = findMaxWidth fields regs cols
    printFields fields cols colsWidth
    printColumns' regs cols colsWidth

-- obtiene el ancho máximo de cada columna
findMaxWidth fields regs cols =
    let regsWidth = findRegsMaxWidth regs cols
        fieldsWidth = findFieldsMaxWidth fields cols
    in mergeWidths regsWidth fieldsWidth
        where
            -- obtiene el ancho máximo de cada columna de los registros
            findRegsMaxWidth regs [] = []
            findRegsMaxWidth regs (col:cols) = wider : findRegsMaxWidth regs cols
                where
                    wider = maximum $ map (\reg -> getValueLength reg col) regs

                    -- devuelve la longitud del valor a mostrar de una columna
                    getValueLength reg pos =
                        let columnValue = extractValueAsStr (reg !! pos)
                        in length columnValue

            -- obtiene el ancho máximo de cada columna de los campos
            findFieldsMaxWidth fields [] = []
            findFieldsMaxWidth fields (col:cols) = nameLen : findFieldsMaxWidth fields cols
                where
                    nameLen = getNameLength fields col

                    -- devuelve la longitud del valor a mostrar de una columna
                    getNameLength fields pos =
                        let columnValue = extractColumnName (fields !! pos)
                        in length columnValue

            -- crea una lista de anchos máximos entre los de los registros y los de los campos
            mergeWidths [] _ = []
            mergeWidths (rw:rws) (fw:fws) = wider : mergeWidths rws fws
                where
                    wider = max rw fw

-- imprime los nombres de los campos seleccionadas, respetando el ancho de cada columna
printFields fields cols colsWidth = do
    let fieldsAsStr = makeFieldString fields cols colsWidth 0
    let lineLength = length fieldsAsStr
    printDashedLine lineLength
    putStr "\ESC[35m"
    putStr fieldsAsStr
    putStrLn "\ESC[0m"

-- imprime los valores de las columnas seleccionadas en una tabla, respetando el ancho de
-- cada columna
printColumns' (reg:regs) cols colsWidth = do
    let regAsStr = makeRegString reg cols colsWidth 0
    let lineLength = length regAsStr
    printDashedLine lineLength
    putStrLn regAsStr
    if null regs
        then do
            printDashedLine lineLength
            putStrLn ""
        else printColumns' regs cols colsWidth

-- imprime la línea de puntos para separar un registro de otro
printDashedLine cant =
    let manySpaces = length tabSpaces
        newCant = cant - manySpaces
    in putStrLn (tabSpaces ++ replicate newCant '-')

makeFieldString fields (col:cols) colsWidth idx = prefix ++ paddedValue ++ next cols
    where
        prefix = tabSpaces ++ "| "

        paddedValue = padTo (colsWidth !! idx) columnName
            where
                columnName = extractColumnName (fields !! col)

        next [] = tabSpaces ++ "|"
        next cols = makeFieldString fields cols colsWidth (idx + 1)

-- devuelve el string formateado correctamente a partir de un registro crudo
makeRegString reg (col:cols) colsWidth idx = prefix ++ paddedValue ++ next cols
    where
        prefix = tabSpaces ++ "| "

        paddedValue = padTo (colsWidth !! idx) (extractValueAsStr (reg !! col))

        next [] = tabSpaces ++ "|"
        next cols = makeRegString reg cols colsWidth (idx + 1)

extractValueAsStr col = case col of
    S value _ -> value
    I value -> show value
    B value -> show value

-- completa con espacios la cantidad de caracteres que le faltan al string para llegar al 
-- ancho deseado
padTo len str = str ++ replicate (len - length str) ' '

-- simula tabulaciones con espacios
tabSpaces = replicate 4 ' '
