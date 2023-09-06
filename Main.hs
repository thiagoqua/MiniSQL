module Main where

import System.Environment (getArgs)
import Parser (parseComm)

import Eval (eval)

main :: IO ()
main = do arg:_ <- getArgs
          run arg

run :: [Char] -> IO ()
run ifile =
    do
    s <- readFile ifile
    case parseComm ifile s of
      Left error -> print error
      Right t    -> eval t    -- imprimir el resultado de evaluar
      --Right t    -> print t           -- imprimir sin evaluar (devuelve AST puro)