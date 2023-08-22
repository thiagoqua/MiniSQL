module AST where
import Data.Text.Internal.Fusion (Step(Skip))

-- Nuestros tipos
type HeterList = [PrimalType]
type Name = String
type TableName = (String,As)
type DatabaseName = String
type ColumnName = (String,As)
type DataType = String
type DataLong = Integer
type From = [TableName]
type Alias = String

-- Estructuras de datos
-- Datos soportados
data PrimalType = S String | I Integer | B Bool
 deriving Show

data Op = Eq 
        | Bt 
        | Bte
        | Lt
        | Lte
        | Neq
 deriving Show

-- COMANDOS
data Command = Seq Command Command              -- (2) el problema puede ser que no encuentra el segundo comando
             | Select SelectQuery
             | CreateDatabase DatabaseName
             | CreateTable Name [ColumnCreation]
             | Insert Name [HeterList]
             | Delete Name Cond
             | Skip                     -- (1) lo ponemos para que corte la ejecución, porque sino no reconoce el eof
 deriving Show

-- CONDICION (WHERE)
data Cond = CoSkip
          | Exp Op Name PrimalType       -- WHERE edad > 18 && nombre = "esteban" -> Exp Bt 'edad' (N 18)
          | CAnd Cond Cond
          | COr Cond Cond
          | CNot Cond
 deriving Show
 
--CREATE TABLE
data ColumnCreation = Column Name DataType DataLong
 deriving Show

-- SELECT

-- Las columnas seleccionadas a partir de un Select
data Columns = Asterisk 
            | Columns [ColumnName]
 deriving Show

-- Consulta completa: Select, From, Where y cláusulas
data SelectQuery = Query Columns From Cond Clause
 deriving Show

-- CLAUSULAS
data Clause = ClSkip
            | OrderBy Name Sort
 deriving Show

data Sort = ASC
          | DESC
 deriving Show

-- ALIAS
data As = ASkip
        | As Alias
 deriving Show
