module Evals.Printers (printAllColumns,printSelectedColumns) where

import Evals.Helpers
import AST

-- FUNCIONES PARA IMPRESIÓN POR PANTALLA

-- imprime todos los registros (asterisco)
printAllColumns fields regs = do
    putStrLn "\nSelect output:\n"
    let colNames = findAllColumnNames fields
    let cols = findIndexes fields colNames
    printColumns (fields : regs) cols

-- busca y devuelve en tuplas todos los nombres de las columnas de la tabla
findAllColumnNames fields = 
    let columns = splitOn '|' fields
    in getName columns
        where 
            getName [] = []
            getName (col:cols) = (extractValue col,ASkip) : getName cols

-- imprime el mensaje y llama a la siguiente
printSelectedColumns fields regs cols = do
    putStrLn "\nSelect output:\n"
    printColumns (fields : regs) cols

-- busca el ancho máximo de las columnas y llama a la siguiente
printColumns regs cols = do
    let colsWidth = findMaxWidth regs cols
    printColumns' regs cols colsWidth

-- encuentra el ancho máximo de cada columna a mostrar 
-- para armar bien la tabla a imprimir
findMaxWidth regs [] = []
findMaxWidth regs (col:cols) = wider : findMaxWidth regs cols
    where 
        wider = maximum $ map (\reg -> getValueLength reg col) regs

-- devuelve la longitud del valor a mostrar de una columna
getValueLength reg pos = 
    let columns = splitOn '|' reg
        columnValue = extractValue (columns !! pos)
    in length columnValue

-- imprime los valores de las columnas seleccionadas en una tabla, respetando el ancho de
-- cada columna
printColumns' (reg:regs) cols colsWidth = do
    let columns = splitOn '|' reg
    let regAsStr = makeRegString columns cols colsWidth 0
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
    in putStrLn (tabSpaces ++ (replicate newCant '-'))

-- devuelve el string formateado correctamente a partir de un registro crudo
makeRegString reg (col:cols) colsWidth idx = prefix ++ paddedValue ++ next cols
    where 
        prefix = tabSpaces ++ "| "
        
        paddedValue = padTo (colsWidth !! idx) (extractValue (reg !! col))

        next [] = " |"
        next cols = makeRegString reg cols colsWidth (idx + 1)

-- completa con espacios la cantidad de caracteres que le faltan al string para llegar al 
-- ancho deseado
padTo len str = str ++ replicate (len - length str) ' '

-- simula tabulaciones con espacios
tabSpaces = replicate 8 ' '

-- extrae de la definición de un valor de la bdd (ej. (hernestino,string,10)) 
-- su valor (ej. "hernestino")
extractValue col = takeWhile (/= ',') (tail col)
