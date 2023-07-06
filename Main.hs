module Main where

import Parser (parseComm)

main::IO()
main = do 
          comando <- getLine
          parseComm comando