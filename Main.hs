module Main where
  
import Parsers.Parser (parseComm)
import Evals.Eval (eval)

import System.Environment (getArgs)
import System.FilePath ( takeExtension )
import System.Directory (doesFileExist)

main :: IO ()
main = do arg:_ <- getArgs
          run arg

run :: [Char] -> IO ()
run ifile = do
    if takeExtension ifile == ".minsql"
    then do exists <- doesFileExist ifile
            if exists
                then do
                  s <- readFile ifile
                  case parseComm ifile s of
                    Left error -> print error
                    Right t    -> eval t              -- imprimir el resultado de evaluar
                    --Right t    -> print t           -- para testing: imprimir sin evaluar (devuelve AST puro)
                else putStrLn "El archivo no existe."
    else putStrLn "La extension del archivo no es válida. Debe ser una extension .minsql."