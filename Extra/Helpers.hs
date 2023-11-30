module Extra.Helpers where

import AST ( Field(..), PrimalType(..) )
import Text.ParserCombinators.Parsec
    ( Parser,
      ParseError,
      char,
      digit,
      noneOf,
      string,
      endBy,
      many1,
      sepBy,
      (<|>),
      parse,
      skipMany,
      try )
import System.IO ( IOMode(ReadWriteMode), hGetLine, openFile )
import System.FilePath ( (<.>), (</>), takeFileName )
import System.Directory (doesFileExist)

-- Funcion para validar base de datos y tabla
validateDirectories name currentDatabase = do
    let tablePath = currentDatabase </> name <.> "txt"
    let dbName = takeFileName currentDatabase
    tableExists <- doesFileExist tablePath
    return (tableExists, dbName, tablePath)

-- Funcion para abrir el archivo de tabla
openTable tablePath = do
            -- Abrir archivo en modo de lectura/escritura
            stream <- openFile tablePath ReadWriteMode
            -- Leer la primera línea y guardarla en una variable
            fieldsAsStr <- hGetLine stream 
            return (stream,fieldsAsStr)

-- Convertir a string la informacion a insertar teniendo en cuenta las reglas definidas
formatData [] = ""
formatData [reg] = formatReg reg ++ "\n"
formatData (reg:regs) = formatReg reg ++ "\n" ++ formatData regs

--Ejemplo: Se formatea -> [S "Esteban",I 20,B True] a "(Esteban,string,20)|(20,integer)|(True,bool)"\n
formatReg [value] =
    "(" ++ getValue value ++ "," ++
    getDataType value ++
    resolveLength value
formatReg (value:values) =
    "(" ++ getValue value ++ "," ++
    getDataType value ++
    resolveLength value ++ "|" ++
    formatReg values

-- Funciones auxiliares de formatReg
getValue (S str _) = str
getValue (B True) = "true"
getValue (B False) = "false"
getValue (I num) = show num

getDataType (S _ _) = "string"
getDataType (B _) = "bool"
getDataType (I _) = "integer"

resolveLength (S str _) = "," ++ show (length str) ++ ")"
resolveLength _ = ")"

-- chequea que la columna name exista en la definición de la tabla
columnExists _ [] = False
columnExists name (f:fields) = name == extractColumnName f || columnExists name fields

extractColumnName field = case field of
  String name _ -> name
  Integer name -> name
  Bool name -> name

-- Función para comparar tipos de datos
compareTypes (S _ _) (String _ _) = True
compareTypes (I _) (Integer _) = True
compareTypes (B _) (Bool _) = True
compareTypes _ _ = False

-- obtenemos los indices de la posicion del nombre de las columnas (de la primera linea)
findIndexes _ [] = []
findIndexes fields ((col,_):cols) = do
    idx <- findColumnPosition col fields 0
    idx : findIndexes fields cols

findColumnPosition :: Monad m => String -> [Field] -> Int -> m Int
findColumnPosition name fields idx = do
    if length fields == idx
        then return (-1)
        else
            if name == extractColumnName (fields !! idx)
                then return idx
                else findColumnPosition name fields (idx + 1)

-- Función para analizar una cadena y obtener la lista de DataType
parseFields = parse dataListParser ""

dataListParser = do
  skipMany (string "\r")
  fieldParser `sepBy` char '|'

-- Analizador para un Field
fieldParser = do
  char '('
  columnName <- many1 (noneOf ",")
  char ','
  dataType <- try (do
    string "string"
    char ','
    lenStr <- many1 digit
    let len = read lenStr
    return (String columnName len))
    <|> try (do
      string "integer"
      return (Integer columnName))
    <|> (do
      string "bool"
      return (Bool columnName))
  char ')'
  return dataType

-- Función para analizar una cadena y obtener la lista de PrimalType

parseContent :: String -> Either ParseError [[PrimalType]]
parseContent = parse linesParser ""

linesParser :: Parser [[PrimalType]]
linesParser = pipesParser `endBy` char '\n'

pipesParser :: Parser [PrimalType]
pipesParser = primalTypeParser `sepBy` char '|'

-- (2,integer)
-- Analizador para un PrimalType
primalTypeParser = do
  char '('
  value <- many1 (noneOf ",")
  char ','
  primalType <- try (do
    string "string"
    char ','
    lenStr <- many1 digit
    let len = read lenStr
    return (S value len))
    <|> try (do
      string "integer"
      return (I (read value :: Integer)))
    <|> (do
      string "bool"
      return (B (parseBool value)))
  char ')'
  return primalType

parseBool "true" = True
parseBool "false" = False
