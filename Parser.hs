module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

-- SELECT * FROM persona WHERE edad > 18   ->    Select (Body (Column Asterisk) [("persona",ASkip)] (Cond (Exp Bt "edad" (N 18))) ClSkip)

sql :: TokenParser u
sql = makeTokenParser (emptyDef   { operators = [">","<",">=","<=","=","!="]
                                  , asterisk  = char '*'
                                  , options   = ["from","where","as","top","order",
                                                 "group","by","asc","desc","into","values"]
                                  , booleans  = ["and","or","not"]
                                  , commands  = ["select","create","database","table","insert","delete"]
                                  }
)

comm::Parser Command
comm = chainl1 comm2

comm2 = try (do reserved sql "select"
                -- TODO
                )
        <|> try (do reserved sql "create"
                   <|> try (do reserved sql "table"
                            -- TODO
                           )
                   <|> try (do reserved sql "database"
                              name <- stringLiteral sql
                              return (CreateDatabase name))                  
                )
        <|> try (do reserved sql "insert"
                -- TODO
                )
        <|> try (do reserved sql "delete"
                -- TODO
                )

parseComm::String -> Either ParseError Command
parseComm = parse comm