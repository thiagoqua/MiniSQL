module AST where

-- CONSULTAS SQL
{-
    SELECT * 
        FROM Table1
    SELECT "column_name" 
        FROM "table_name";
    SELECT "column_name1", "column_name2" 
        FROM "table_name";
    SELECT * 
        FROM “table_name” 
        WHERE “column_name” = “nombre”
    SELECT * 
        FROM “table_name” 
        WHERE “column_name” = “nombre” AND 
              “column_name1” = “nombre” 
    SELECT * 
        FROM “table_name” 
        WHERE “column_name” = “nombre” OR 
              “column_name1” = “nombre”
    SELECT * 
        FROM “table_name” 
        WHERE NOT “column_name” = “nombre”
    SELECT pv.ProductID, v.BusinessEntityID, v.Name 
        FROM Purchasing.ProductVendor AS pv, 
             Purchasing.Vendor AS v 
        WHERE pv.BusinessEntityID = v.BusinessEntityID
    SELECT TOP “number” * 
        FROM “table_name”
    SELECT * 
        FROM “table_name” 
        ORDER BY “column_name” BY ASC
    SELECT * 
        FROM “table_name” 
        ORDER BY “column_name” BY DESC
    SELECT * 
        FROM “table_name” 
        GROUP BY “nombre”
    
    CREATE DATABASE "db_name”;
    CREATE TABLE table_name ("column_name" dataType(largo));
    
    INSERT INTO “table_name” 
        VALUES (“data1", "data2", "data3”);
    
    DELETE FROM "table_name";
    DELETE FROM "table_name"
        WHERE condition;
-}

-- Nuestros tipos
type HeterList = [Types]
type Name = String
type TableName = String
type DatabaseName = String
type ColumnName = String
type DataType = String
type DataLong = Int
type Alias = String
type Op = Char   -- >,<,=,>=,<=,!=

-- Estructuras de datos
-- Datos soportados
data Types = S String | N Integer | B Bool | F Float


-- Expresiones Booleanas
data BoolExp = BTrue
             | BFalse
             | Eq IntExp IntExp
             | And BoolExp BoolExp
             | Or BoolExp BoolExp
             | Not BoolExp
 deriving (Show,Eq)

data IntExp = Const Integer
 deriving (Show,Eq)

-- COMANDOS
data Command = Select SelectBody
             | CreateDatabase DatabaseName
             | CreateTable TableName [ColumnCreation]
             | Insert ColumnName [HeterList]
             | Delete TableName Cond
 deriving Show

--CONDICION
------------------------- TODO ----------------
data Cond = Skip
          | Exp Op Name Types

--CREATE TABLE
data ColumnCreation = ColumnString ColumnName DataType DataLong
                    | ColumnOthers ColumnName DataType
 deriving Show

-- SELECT
-- OPCIONES
data SelectOpc = Column ColumnOptions
               | Top Integer ColumnOptions

data ColumnOptions = Asterisk 
                   | Columns [ColumnName]

-- CUERPO
data SelectBody = Body SelectOpc TableName Cond
 deriving Show

------------------------- TODO ----------------
data OrderBy = OrderBy_ String By
 deriving Show

data GroupBy = GroupBy_ String
 deriving Show

data By = ASC
        | DESC
 deriving Show


data As = As_ ColumnName Alias
 deriving Show

-- data From = From_ String
--           | From1_ [String]
--           | From2_ [As]
--  deriving Show
