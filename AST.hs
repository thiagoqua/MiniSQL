module AST where

-- CONSULTAS SQL
{-
    SELECT * FROM Table1
    SELECT "column_name" FROM "table_name";
    SELECT "column_name1", "column_name2" FROM "table_name";
    SELECT * FROM “table_name” WHERE “column_name” = “nombre”
    SELECT * FROM “table_name” WHERE “column_name” = “nombre” AND “column_name1” = “nombre”
    SELECT * FROM “table_name” WHERE “column_name” = “nombre” OR “column_name1” = “nombre”
    SELECT * FROM “table_name” WHERE NOT “column_name” = “nombre”
    SELECT pv.ProductID, v.BusinessEntityID, v.Name FROM Purchasing.ProductVendor AS pv, Purchasing.Vendor AS v WHERE pv.BusinessEntityID=v.BusinessEntityID
    SELECT TOP “number” * FROM “table_name”
    SELECT * FROM “table_name” ORDER BY “column_name” BY ASC
    SELECT * FROM “table_name” ORDER BY “column_name” BY DESC
    SELECT * FROM “table_name” GROUP BY “nombre”
    CREATE DATABASE “NombreBDD”;
    CREATE TABLE “table_name” (nombre_columna tipodato(largo));
    INSERT INTO “table_name” VALUES (“date1, date2, date3”);
    DELETE FROM table_name;
    DELETE FROM table_name WHERE condition;
-}

data IntExp = Const Integer
 deriving (Show,Eq)

-- Expresiones Booleanas
data BoolExp = BTrue
             | BFalse
             | Eq IntExp IntExp
             | And BoolExp BoolExp
             | Or BoolExp BoolExp
             | Not BoolExp
 deriving (Show,Eq)

-- COMANDOS
data Command = Select SelQueries
             | CreateDatabase String
             | CreateTable String [CreateTableQuery]
             | Insert String [String]
             | Delete Del
 deriving Show

--CREATE TABLE
data CreateTableQuery = CreateTable_ String String Int
                      | CreateTable1_ String String
 deriving Show

--SELECT
data SelQueries = SelQ_ Asterisk
                | SelQ1_ ColumnMaster
                | SelQ2_ Top From
 deriving Show

data Asterisk = Ast_ Char From
              | Ast1_ Char From Where
              | Ast2_ Char From GroupBy
              | Ast3_ Char From OrderBy
 deriving Show

data OrderBy = OrderBy_ String By
 deriving Show

data By = ASC
        | DESC
 deriving Show

data GroupBy = GroupBy_ String
 deriving Show

data Top = Top_ IntExp
 deriving Show

data ColumnMaster = CM_ Column From Where
                  | CM1_ Column From
 deriving Show

data Column = Col_ [String]
 deriving Show

data From = From_ String
          | From1_ [String]
          | From2_ [As]
 deriving Show

data As = As_ String String
 deriving Show

data Where = Where_ BoolExp
 deriving (Show,Eq)

--DELETE
data Del = Del_ From Where
         | Del1_ From
 deriving Show
