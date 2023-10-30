module Evals.EvalDelete (evalDelete) where
    
import AST (Cond (..), Op (Eq, Lt, Bt, Lte, Bte, Neq), PrimalType (S, B, I))

import Extra.Helpers (compareTypes, splitOn)
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
import Control.Monad
import Data.Char
import Data.Array (indices)

evalDelete name condition currentDatabase = do
    let tablePath = currentDatabase </> name <.> "txt"
    tableExists <- doesFileExist tablePath
    -- Revisar que la tabla exista
    if tableExists
        then
            case condition of
                CoSkip -> do
                    -- Abrir archivo en modo de lectura/escritura
                    stream <- openFile tablePath ReadWriteMode
                    -- Leer la primera línea y guardarla en una variable
                    fields <- hGetLine stream
                    -- Posicionar el puntero del archivo al principio
                    hSeek stream AbsoluteSeek 0
                    -- Eliminar el contenido actual del archivo
                    hSetFileSize stream 0
                    -- Escribir la primera linea de vuelta en el archivo
                    hPutStr stream (fields ++ "\n")
                    -- Cerrar el archivo
                    hClose stream
                    putStrLn $ "Registros eliminados de la tabla '" ++ name ++ "' en la base de datos '" ++ currentDatabase ++ "'."
                condition -> do
                    stream <- openFile tablePath ReadWriteMode
                    fields <- hGetLine stream
                    -- Guardar todo el contenido del archivo, menos la primera linea
                    contents <- hGetContents stream
                    -- Dividir el contenido en líneas
                    let registers = lines contents
                    -- registersToInsert contiene los registros que NO cumplen con la condicion
                    -- Como no cumplen la condicion, se guardaran posteriormente en el archivo
                    case verifCond registers condition fields of
                        Right registersToInsert -> do
                            -- Crear un nuevo archivo temporal para escribir los registros actualizados
                            let tempTablePath = tablePath ++ ".temp"
                            -- Abrir el nuevo archivo en modo escritura
                            newStream <- openFile tempTablePath WriteMode
                            -- Escribir la primera línea (que contiene los campos) en el nuevo archivo
                            hPutStrLn newStream fields
                            -- Escribir los registros actualizados en el nuevo archivo
                            mapM_ (hPutStrLn newStream) registersToInsert
                            -- Cerrar el nuevo archivo
                            hClose newStream
                            -- Renombrar el archivo temporal para reemplazar el original
                            renameFile tempTablePath tablePath
                            putStrLn "Registros borrados exitosamente."
                        Left error -> putStrLn error
        else do
            putStrLn $ "La tabla '" ++ name ++ "' no existe en la base de datos '" ++ currentDatabase ++ "'."


-- Verifica que se cumpla la condicion en todos los registros
verifCond [] _ _ = Right []
verifCond (x:xs) cond fields = 
    case verifCond' x cond fields of
        Right True -> verifCond xs cond fields
        Right False -> (x :) <$> verifCond xs cond fields
        Left errorMsg -> Left errorMsg
