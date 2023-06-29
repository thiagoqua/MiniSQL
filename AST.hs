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
type From = [(TableName,As)]
type Alias = String
type Op = Char   -- >,<,=,>=,<=,!=

-- Estructuras de datos
-- Datos soportados
data Types = S String | N Integer | B Bool | F Float
 deriving Show


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

-- CONDICION
data Cond = CTrue
          | CFalse
          | CoSkip
          | Exp Op Name Types       -- WHERE edad > 18 && nombre = "esteban" -> Exp '>' 'edad' (N 18)
          | CAnd Cond Cond
          | COr Cond Cond
          | CNot Cond
 deriving Show

--CREATE TABLE
data ColumnCreation = ColumnString ColumnName DataType DataLong
                    | ColumnOthers ColumnName DataType
 deriving Show

-- SELECT
-- OPCIONES
data SelectOpc = Column ColumnOptions
               | Top Integer ColumnOptions
 deriving Show

data ColumnOptions = Asterisk 
                   | Columns [ColumnName]
 deriving Show

-- CUERPO
data SelectBody = Body SelectOpc From Cond Clause
 deriving Show

-- CLAUSULAS
data Clause = ClSkip
            | OrderBy ColumnName Sort
            | GroupBy ColumnName
 deriving Show

data Sort = ASC
          | DESC
 deriving Show

-- ALIAS
data As = ASkip
        | As Alias
 deriving Show
