module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST
import Data.Char (toLower)
import Debug.Trace (traceM)
import Text.XHtml (table)
import Data.List (isInfixOf)


newParser :: Parser a -> Parser a
newParser p = do
                  whiteSpace sql
                  t <- p
                  skipMany (char '\r')
                  eof
                  return t

sql :: TokenParser u
sql = makeTokenParser (emptyDef   { commentLine = "--"
                                  , identStart = letter         -- sirve para empezar los alias de tablas y columnas
                                  , identLetter = letter        -- sirve para reconocer el resto de caracteres de los alias de tablas y columnas
                                  , reservedOpNames = [">","<",">=","<=","=","!=","*","-",",",";"]
                                  , reservedNames = ["from","where","as","order by",
                                                 "asc","desc","into","values",
                                                 "and","or","not",
                                                 "select","create","database","table",
                                                 "insert","delete","use",
                                                 "String","Integer","Bool"]
                                  }
                      )


--CREATE TABLE

datatypes = try (do reserved sql "string"
                    return "string"     -- acá devolvemos el nombre del tipo de dato, no el valor
                                        -- despues en el insert hay que definir un valor para el tipo (ej. I 3)
                )
            <|> try (do reserved sql "integer"
                        return "integer"
                )
            <|> try (do reserved sql "bool"
                        return "bool"
                )

colCreate = chainl1 colCreate' (try (do reservedOp sql ","
                                        return (++))
                               )

colCreate' = try (do columnName <- identifier sql
                     reservedOp sql "-"
                     dataType <- datatypes
                     if dataType == "string"
                        then do reservedOp sql "-"
                                dataLong <- integer sql
                                return [Column columnName dataType dataLong]
                        else return [Column columnName dataType 0]
                 )


--USE Y CREATE DATABASE

first = try cdbParser <|> useParser

cdbParser = do reserved sql "create"
               reserved sql "database"
               name <- identifier sql
               reservedOp sql ";"
               using <- resolveUse
               return (CreateDatabase name using)

resolveUse =    do using <- useParser
                   return using
                <|> do anyChar
                       fail "Comandos no válidos después de CREATE DATABASE"
                       <|> return Skip

useParser = do reserved sql "use"
               name <- identifier sql
               reservedOp sql ";"
               command <- commSep
               return (Use name command)


--INSERT

valueParser = try (do reserved sql "true"
                      return (B True)
                  )
              <|> try (do reserved sql "false"
                          return (B False))
              <|> try (do value <- integer sql
                          return (I value))
              <|> try (do string <- stringLiteral sql
                          return (S string))

valuesListParser = sepBy valueParser (char ',' >> whiteSpace sql) --para borrar los espacios después de la coma
    --llama a una funcion para analizar cada dato
    -- y separa por coma cada dato

parenValuesParser = between (char '(') (char ')' >> spaces) valuesListParser  --toma lo que está entre parentesis
                                                                    -- y llama a una funcion para analizar el contenido

colInsert = sepBy parenValuesParser (spaces >> char ',' >> spaces) --para borrar los espacios antes y después de la coma
  --separa por las comas generales
  -- y llama a una funcion para analizar lo que está entre parentesis


--SELECT
columns = try (do reservedOp sql "*"
                  return Asterisk
              )
          <|> (do columnSelect <- colSelect
                  return (Columns columnSelect))

aliasName = do col <- identifier sql
               try (do reserved sql "as"
                       alias <- identifier sql
                       return (col, As alias)
                   )
                   <|> return (col, ASkip)

colSelect = sepBy aliasName (spaces >> char ',' >> spaces)

clause = (do reserved sql "order by"
             name <- identifier sql
             ord <- order
             return (OrderBy name ord)
         )
         <|> return ClSkip

order = try (do reserved sql "asc"
                return ASC
            )
        <|> (do reserved sql "desc"
                return DESC
            )

-- COMM PRINCIPAL
-- funcion que se encarga de combinar expresiones separadas por un ;
commSep::Parser Command
commSep = do cmdsList <- endBy comm2 (reservedOp sql ";")
             return (foldl1 Seq cmdsList)

--COMM DE LOS COMANDOS
comm2 = try (do reserved sql "select"
                col <- columns
                reserved sql "from"
                colTable <- colSelect
                cond <- condition
                cl <- clause
                return (Select col colTable cond cl)
        )
        <|> try (do reserved sql "create"
                    reserved sql "table"
                    tableName <- identifier sql
                    columns <- parens sql colCreate
                    return (CreateTable tableName columns)
            )
        <|> try (do reserved sql "insert"
                    reserved sql "into"
                    tableName <- identifier sql
                    reserved sql "values"
                    values <- colInsert     --llamamos a colInsert para dividir cada registro
                    return (Insert tableName values) --guardamos la lista generada adentro de otra lista (chequear)
            )
        <|> try (do reserved sql "delete"
                    reserved sql "from"
                    tableName <- identifier sql
                    cond <- condition
                    return (Delete tableName cond)
            )

--DELETE

condition = (do reserved sql "where"
                cond <- boolexp
                return cond
            )
            <|> return CoSkip

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


--FUNCION PARA PARSEAR

loweize::String -> String
loweize = map toLower

--FUNCION PARA VER CUAL ES LA INPUT RESTANTE POR CONSUMIR


--seeNext = do input <- lookAhead (manyTill anyChar eof)
--             _ <- traceM ("The next is " ++ show input)
--             return input


parseComm :: SourceName -> String -> Either ParseError Command
parseComm source input = parse (newParser first) source (loweize input)