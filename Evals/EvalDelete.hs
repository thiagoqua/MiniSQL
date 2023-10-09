module Evals.EvalDelete (evalDelete) where
    
import AST (Cond (..), Op (Eq, Lt, Bt, Lte, Bte, Neq), PrimalType (S, B, I))

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
import Control.Monad
import Data.Char
import Data.Array (indices)

evalDelete name condition currentDatabase = do
    -- Revisar la BDD actual
    case currentDatabase of
        Just dbName -> do
            let tablePath = dbName </> name <.> "txt"
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
                            putStrLn $ "Registros eliminados de la tabla '" ++ name ++ "' en la base de datos '" ++ dbName ++ "'."
                        condition -> do
                            stream <- openFile tablePath ReadWriteMode
                            fields <- hGetLine stream
                            -- Guardar todo el contenido del archivo, menos la primera linea
                            contents <- hGetContents stream
                            -- Dividir el contenido en líneas
                            let registers = lines contents
                            -- registersToInsert contiene los registros que NO cumplen con la condicion
                            -- Como no cumplen la condicion, se guardaran posteriormente en el archivo
                            let registersToInsert = verifCond registers condition fields
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
                else do
                    putStrLn $ "La tabla '" ++ name ++ "' no existe en la base de datos '" ++ dbName ++ "'."
        Nothing -> do
                    putStrLn "No se ha seleccionado una base de datos."


-- Verifica que se cumpla la condicion en todos los registros
verifCond [] _ _ = []
verifCond (x:xs) cond fields = if verifCond' x cond fields
    then verifCond xs cond fields
    else x : verifCond xs cond fields
