module AST where
import Data.Text.Internal.Fusion (Step(Skip))

-- Tipos
type HeterList = [PrimalType]
type Name = String
type TableName = (String,As)
type DatabaseName = String
type ColumnName = (String,As)
type DataType = String
type DataLong = Integer
type From = [TableName]
type Alias = String

-- Tipos de datos soportados
data PrimalType = S String | I Integer | B Bool
 deriving Show

-- Operadores de comparacion
data Op = Eq 
        | Bt 
        | Bte
        | Lt
        | Lte
        | Neq
 deriving Show

-- Comandos
data Command = Use DatabaseName Command
             | Seq Command Command
             | Select Columns From Cond Clause
             | CreateDatabase DatabaseName Command
             | CreateTable Name [ColumnCreation]
             | Insert Name [HeterList]
             | Delete Name Cond
             | Skip
 deriving Show

-- Condicion (Where)
data Cond = CoSkip
          | Exp Op Name PrimalType
          | CAnd Cond Cond
          | COr Cond Cond
          | CNot Cond
 deriving Show
 
--Create Table
data ColumnCreation = Column Name DataType DataLong
 deriving Show

-- Select
data Columns = Asterisk                 -- *
            | Columns [ColumnName]      -- Una o varias columnas
 deriving Show

-- Clausulas
data Clause = ClSkip
            | OrderBy Name Sort
 deriving Show

data Sort = ASC
          | DESC
 deriving Show

-- Alias
data As = ASkip
        | As Alias
 deriving Show
