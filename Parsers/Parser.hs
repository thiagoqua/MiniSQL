{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parsers.Parser where

import AST ( Command(..) )
import Parsers.CommandParser
    ( conditionParser,
      clauseParser,
      columnsParser,
      insertColumnParser,
      createColumnParser,
      sql ) 

import Text.ParserCombinators.Parsec
    ( Parser,
      ParseError,
      anyChar,
      char,
      endBy,
      eof,
      (<|>),
      parse,
      skipMany,
      SourceName,
      try )
import Text.Parsec.Token
    ( GenTokenParser(identifier, whiteSpace, reservedOp, parens,
                     reserved) )
import Text.Parsec.Language (emptyDef)

import Data.Char (toLower)
import Debug.Trace (traceM)
import Text.XHtml (table)

-- Punto de entrada del parser
parseComm :: SourceName -> String -> Either ParseError Command
parseComm fileName input = do
    let lowerInput = loweize input
    let cleanerParser = cleanInput mainParser
    parse cleanerParser fileName lowerInput

-- Se encarga de quitar espacios en blanco y retornos de carro
cleanInput :: Parser a -> Parser a
cleanInput p = do
                  whiteSpace sql
                  t <- p
                  skipMany (char '\r')
                  eof
                  return t

-- Función que convierte la entrada en minusculas
loweize::String -> String
loweize = map toLower

-- Parser que se encarga de llamar a los otros parsers
-- Primero revisa que existan las sentencias "create database" y "use"
-- De acuerdo a las reglas definidas, una vez que las encuentra, procede con el resto de sentencias
mainParser = try createDatabaseParser <|> useParser

-- Parsea "create database"
createDatabaseParser = do reserved sql "create"
                          reserved sql "database"
                          name <- identifier sql
                          reservedOp sql ";"
                          next <- resolveNext
                          return (CreateDatabase name next)

-- Verifica si existe un "use" despues de "create database", o si hay otro "create database"
resolveNext =    do useParser
                 <|> do createDatabaseParser
                 <|> do anyChar
                        fail "Comandos no válidos después de CREATE DATABASE"
                        <|> return Skip

-- Parsea "use"
useParser = do reserved sql "use"
               name <- identifier sql
               reservedOp sql ";"
               command <- commSeparator
               return (Use name command)

-- Parser que se encarga de combinar comandos separadas por un ;
commSeparator :: Parser Command
commSeparator = do cmdsList <- endBy commands (reservedOp sql ";")
                   case cmdsList of
                        [] -> return Skip
                        list -> return (foldl1 Seq list)

-- Parser de comandos
commands = try (do reserved sql "select"
                   columns <- columnsParser
                   reserved sql "from"
                   table <- identifier sql
                   condition <- conditionParser
                   clause <- clauseParser
                   return (Select columns table condition clause)
        )
        <|> try (do reserved sql "create"
                    reserved sql "table"
                    tableName <- identifier sql
                    columns <- parens sql createColumnParser
                    return (CreateTable tableName columns)
            )
        <|> try (do reserved sql "insert"
                    reserved sql "into"
                    tableName <- identifier sql
                    reserved sql "values"
                    values <- insertColumnParser
                    return (Insert tableName values)
            )
        <|> try (do reserved sql "delete"
                    reserved sql "from"
                    tableName <- identifier sql
                    cond <- conditionParser
                    return (Delete tableName cond)
            )
