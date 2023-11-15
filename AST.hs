module AST where
import Data.Text.Internal.Fusion (Step(Skip))

-- Tipos
type HeterList = [PrimalType]
type Name = String
type TableName = String
type DatabaseName = String
type ColumnName = (Name,As)
type DataLong = Integer
type Alias = String

-- definicion de la primera linea (fields). se usa en el eval
data Field = String Name DataLong | Integer Name | Bool Name
 deriving Show

-- Tipos de datos soportados ALMACENA VALORES
-- se usa en el parser para facilitar el uso de los valores de tipo string
data PrimalType = S String DataLong | I Integer | B Bool
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
data ColumnCreation = Column Name Field
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
