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

-- Definicion de la primera linea del archivo (campos/fields).
data Field = String Name DataLong | Integer Name | Bool Name
 deriving Show

-- Almacena valores de un registro
-- Sirve para facilitar el uso de los valores de tipo string
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

--Create Table
data ColumnCreation = Column Name Field
 deriving Show

-- Select
data Columns = Asterisk                 -- *
            | Columns [ColumnName]      -- Una o varias columnas
 deriving Show

-- Condicion (Where)
data Cond = CoSkip
          | Exp Op Name PrimalType
          | CAnd Cond Cond
          | COr Cond Cond
          | CNot Cond
 deriving Show

-- Clausulas
data Clause = ClSkip
            | OrderBy Name Sort
 deriving Show

-- Order By
data Sort = ASC
          | DESC
 deriving Show

-- Alias
data As = ASkip
        | As Alias
 deriving Show
