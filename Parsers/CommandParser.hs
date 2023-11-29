{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# Language FlexibleContexts #-}
module Parsers.CommandParser where

import AST

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)

-- Definicion del lenguaje
sql :: TokenParser u
sql = makeTokenParser (emptyDef   { commentLine = "--"
                                  , identStart = letter
                                  , identLetter = letter
                                  , reservedOpNames = [">","<",">=","<=","=","!=","*","-",",",";"]
                                  , reservedNames = ["from","where","as","order by",
                                                 "asc","desc","into","values",
                                                 "and","or","not",
                                                 "select","create","database","table",
                                                 "insert","delete","use",
                                                 "String","Integer","Bool"]
                                  }
                      )


-- SELECT

columnsParser = try (do reservedOp sql "*"
                        return Asterisk
                )
                <|> (do columnSelect <- columnsParser'
                        case columnSelect of
                            [] -> fail "No hay columnas seleccionadas."
                            cols -> return (Columns cols)
                     )

aliasName = do col <- identifier sql
               try (do reserved sql "as"
                       alias <- identifier sql
                       return (col, As alias)
                   )
                   <|> return (col, ASkip)

columnsParser' = commaSeparatorParser aliasName

clauseParser = (do reserved sql "order by"
                   name <- identifier sql
                   ord <- order
                   return (OrderBy name ord)
         )
         <|> return ClSkip

order = do reserved sql "asc"
           return ASC
        <|> do reserved sql "desc"
               return DESC


-- CREATE TABLE

createColumnParser = chainl1 create (try (do reservedOp sql ","
                                             return (++))
                                    )

create = try (do columnName <- identifier sql
                 reservedOp sql "-"
                 dataTypeAsString <- datatypes
                 dataType <- checkDataType columnName dataTypeAsString
                 return [Column columnName dataType]
             )

checkDataType columnName dataTypeAsString
  | dataTypeAsString == "string" = do reservedOp sql "-"
                                      dataLong <- integer sql
                                      if dataLong < 1
                                            then fail "bigger than 0"
                                            else return (String columnName dataLong)
  | dataTypeAsString == "integer" = return (Integer columnName)
  | otherwise = return (Bool columnName)

datatypes = do reserved sql "string"
               return "string"
            <|> do reserved sql "integer"
                   return "integer"
            <|> do reserved sql "bool"
                   return "bool"


-- INSERT

-- Borra espacios antes y despues de la coma
-- Luego, separa por comas
-- Finalmente, llama a un parser para analizar lo que está entre parentesis
insertColumnParser = commaSeparatorParser parenValuesParser

-- Toma el contenido que está entre parentesis
-- Luego, llama a un parser para analizar el contenido
parenValuesParser = between (char '(') (char ')' >> spaces) valuesListParser

-- Borra los espacios después de la coma
-- Llama a un parser para analizar cada dato
-- Finalmente, separa por coma cada dato
valuesListParser = sepBy valueParser (char ',' >> whiteSpace sql)

valueParser = do reserved sql "true"
                 return (B True)
              <|> do reserved sql "false"
                     return (B False)
              <|> do value <- integer sql
                     return (I value)
              <|> do string <- stringLiteral sql
                     if null string
                            then fail "empty strings are not allowed"
                            else do let len = toInteger $ length string
                                    return (S string len)

-- CONDICIÓN (DELETE/SELECT)

conditionParser = (do reserved sql "where"
                      boolexp
                  )
                  <|> return CoSkip

-- Se usa chainl1 para eliminar la recursion izquierda y evaluar la condición de izquierda a derecha
boolexp  = chainl1 boolexp2 (try (do reserved sql "or"
                                     return COr))

boolexp2 = chainl1 boolexp3 (try (do reserved sql "and"
                                     return CAnd))

boolexp3 = try (parens sql boolexp)
           <|> try (do reserved sql "not"
                       b <- boolexp3
                       return (CNot b))
           <|> compValues

compValues = try (do s <- identifier sql
                     o <- op
                     v <- valueParser
                     return (Exp o s v))
             <|> try (do v <- valueParser
                         o <- op
                         s <- identifier sql
                         return (Exp o s v))

op = try (do reservedOp sql "="
             return Eq)
     <|> try (do reservedOp sql "<"
                 return Lt)
     <|> try (do reservedOp sql ">"
                 return Bt)
     <|> try (do reservedOp sql ">="
                 return Bte)
     <|> try (do reservedOp sql "<="
                 return Lte)
     <|> try (do reservedOp sql "!="
                 return Neq)


-- Funciones auxiliares
commaSeparatorParser parser = sepBy parser (spaces >> char ',' >> spaces)
