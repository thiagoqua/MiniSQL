module Evals.EvalDelete (evalDelete) where

import AST (Cond (..), Op (Eq, Lt, Bt, Lte, Bte, Neq), PrimalType (S, B, I))

import Extra.Helpers (splitOn,parseFields,parseContent)
--import Evals.EvalCondition (verifCond')

import System.Directory ( doesFileExist, renameFile )
import System.FilePath ( (<.>), (</>) )
import System.IO
    ( hClose,
      hSeek,
      hSetFileSize,
      hGetLine,
      hPutStr,
      hIsEOF,
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
                    fieldsAsStr <- hGetLine stream
                    case parseFields fieldsAsStr of
                        Right _ -> do
                            -- Revisar que haya contenido despues de la primera linea
                            isEof <- hIsEOF stream
                            if not isEof
                            then do
                                -- Posicionar el puntero del archivo al principio
                                hSeek stream AbsoluteSeek 0
                                -- Eliminar el contenido actual del archivo
                                hSetFileSize stream 0
                                -- Escribir la primera linea de vuelta en el archivo
                                hPutStr stream (fieldsAsStr ++ "\n")
                                -- Cerrar el archivo
                                hClose stream
                                putStrLn $ "Registros eliminados de la tabla '" ++ name ++ "' en la tabla '" ++ currentDatabase ++ "'."
                            else do
                                hClose stream
                                putStrLn "No hubo registros eliminados."
                        Left _ -> putStrLn "Hubo un error interno."
                condition -> do
                    stream <- openFile tablePath ReadWriteMode
                    fieldsAsStr <- hGetLine stream
                    case parseFields fieldsAsStr of
                        Right fields -> do
                            -- Guardar todo el contenido del archivo, menos la primera linea
                            contentsAsStr <- hGetContents stream
                            case parseContent contentsAsStr of
                                Right contents -> 
                                    -- registersToInsert contiene los registros que NO cumplen con la condicion
                                    -- Como no cumplen la condicion, se guardaran posteriormente en el archivo
                                    case verifCond contents condition fields of
                                        Right registersToInsert -> do
                                            -- si los arreglos son distintos, significa que no hubo registros que cumplan la condición
                                            if length contents /= length registersToInsert
                                            then do
                                                -- Crear un nuevo archivo temporal para escribir los registros actualizados
                                                let tempTablePath = tablePath ++ ".temp"
                                                -- Abrir el nuevo archivo en modo escritura
                                                newStream <- openFile tempTablePath WriteMode
                                                -- Escribir la primera línea (que contiene los campos) en el nuevo archivo
                                                hPutStrLn newStream fieldsAsStr
                                                -- Escribir los registros actualizados en el nuevo archivo
                                                mapM_ (hPutStrLn newStream) registersToInsert
                                                -- Cerrar el nuevo archivo y el archivo original
                                                hClose newStream
                                                hClose stream
                                                -- Renombrar el archivo temporal para reemplazar el original
                                                renameFile tempTablePath tablePath
                                                putStrLn "Registros borrados exitosamente."
                                            else do hClose stream
                                                    putStrLn "No hubo registros eliminados."
                                        Left error -> putStrLn error
                                Left _ -> putStrLn "Hubo un error interno."
                        Left _ -> putStrLn "Hubo un error interno."
                    
        else do
            putStrLn $ "La tabla '" ++ name ++ "' no existe en la base de datos '" ++ currentDatabase ++ "'."

-- Verifica que se cumpla la condicion en todos los registros
verifCond [] _ _ = Right []
verifCond (x:xs) cond fields = Right ["lcdtm"]
--    case verifCond' x cond fields of
        --Right True -> verifCond xs cond fields
        --Right False -> (x :) <$> verifCond xs cond fields
        --Left errorMsg -> Left errorMsg
