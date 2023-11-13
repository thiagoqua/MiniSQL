module Extra.Helpers where

import AST
import Data.List (isInfixOf)
import Text.ParserCombinators.Parsec
import Text.Parsec (endOfLine)

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

splitOn _ [] = []
splitOn delimiter list = let (first, rest) = break (== delimiter) list
                         in first : case rest of
                             [] -> []
                             (_:xs) -> splitOn delimiter xs

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

-- Analizador para un DataType
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
      char ','
      _ <- digit
      return (Integer columnName))
    <|> (do
      string "bool"
      char ','
      _ <- digit
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
