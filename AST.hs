module AST where

data BoolExp = BTrue
             | BFalse
             | Eq IntExp IntExp
             | And BoolExp BoolExp
             | Or BoolExp BoolExp
 deriving Show

--CREATE TABLE table_name (column_name datatype(length))
data CTQuery = Val String String Int
            | Val String String
 deriving Show

data Where = Where_ BoolExp
data From = From_ String

--DELETE
data Del = Del_ From Where
         | Del_ From

data Command = Select
             | CreateDatabase String
             | CreateTable String [CTQuery]
             | Insert From [String]               --From == Into -> en este caso
             | Delete Del
 deriving Show
