module AST where
import Data.Text.Internal.Fusion (Step(Skip))

-- Tipos
type HeterList = [PrimalType]
type Name = String
type TableName = String
type DatabaseName = String
type ColumnName = (String,As)   -- cambiar por (Name, As)
type DataType = String          -- cambiar por data DataType = String Int | Integer | Bool (para parsear en el eval)
type DataLong = Integer
type Alias = String

-- Tipos de datos soportados
data PrimalType = S String | I Integer | B Bool         -- cambiar por S String Int (para el parser)
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
             | Select Columns TableName Cond Clause
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
