import Data.Vect

infixr 5 .+.

data Schema =
  SString
  | SInt
  | SChar
  | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

data Command : Schema -> Type where
  SetSchema : (newSchema : Schema) -> Command schema
  Add : SchemaType schema -> Command schema
  Get : Integer -> Command schema
  Print : Command schema
  Quit : Command schema

addToStore :
  (store : DataStore) ->
  SchemaType (schema store) ->
  DataStore
addToStore (MkData schema size items) item =
  MkData schema _ (addToData items)
  where
    addToData : Vect oldSize (SchemaType schema) -> Vect (S oldSize) (SchemaType schema)
    addToData [] = [item]
    addToData (x :: xs) = x :: addToData xs

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (SchemaType (schema store))
getEntry pos (MkData schema size items) =
  (\id => index id items) <$> integerToFin pos size

showEntry : SchemaType schema -> String
showEntry {schema = SString} x = show x
showEntry {schema = SInt} x = show x
showEntry {schema = SChar} x = show x
showEntry {schema = (x .+. y)} (a, b) = showEntry a ++ ", " ++ showEntry b

setSchema : (store : DataStore) -> Schema -> Either String DataStore
setSchema store schema =
  if (size store) == 0
  then Right (MkData schema _ [])
  else Left "Can't change schema of a non-empty store!"

parseSchema : String -> Either String Schema
parseSchema schemaStr =
  parseSchemaList (words schemaStr)
  where
  parseSchemaWord : String -> Either String Schema
  parseSchemaWord "String" = Right SString
  parseSchemaWord "Int" = Right SInt
  parseSchemaWord "Char" = Right SChar
  parseSchemaWord word = Left ("Unknown schema keyword '" ++ word ++ "'")

  parseSchemaList : (List String) -> Either String Schema
  parseSchemaList [] = Left "Empty schema definition!"
  parseSchemaList (word :: []) = parseSchemaWord word
  parseSchemaList (word :: xs) =
    do
    leftSchema <- parseSchemaWord word
    rightSchema <- parseSchemaList xs
    Right (leftSchema .+. rightSchema)

parsePrefix : (schema : Schema) -> (input : String) -> Maybe (SchemaType schema, String)
parsePrefix SString input =
  getQuoted (unpack input)
  where
  getQuoted : (List Char) -> Maybe (String, String)
  getQuoted ('"' :: xs) =
    case span (/= '"') xs of
      (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
      _ => Nothing
  getQuoted _ = Nothing
parsePrefix SInt input =
  case span isDigit input of
    ("", rest) => Nothing
    (num, rest) => Just (cast num, ltrim rest)
parsePrefix SChar input =
  case unpack input of
    (c :: rest) => Just (c, ltrim (pack rest))
    [] => Nothing
parsePrefix (x .+. y) input =
  do
  (xv, rest_x) <- parsePrefix x input
  (yv, rest_y) <- parsePrefix y rest_x
  Just ((xv, yv), rest_y)

parseBySchema : (schema : Schema) -> (args : String) -> Either String (SchemaType schema)
parseBySchema schema args =
  case parsePrefix schema args of
    Just (result, "") => Right result
    Just (result, extra) => Left ("Unexpected input: " ++ extra)
    Nothing => Left "Parsing error."

parseCommand :
  (schema : Schema) -> (cmd : String) -> (args : String) ->
  Either String (Command schema)
parseCommand _ "schema" input =
  SetSchema <$> parseSchema input
parseCommand schema "add" args =
  Add <$> parseBySchema schema args
parseCommand schema "get" "" = Right Print
parseCommand schema "get" idxs =
  if all isDigit (unpack idxs)
  then Right (Get (cast idxs))
  else Left ("'" ++ idxs ++ "' is not a valid index!")
parseCommand schema "quit" _ = Right Quit
parseCommand _ cmd _ = Left ("Unknown command '" ++ cmd ++ "'")

parse : (schema : Schema) -> (input : String) -> Either String (Command schema)
parse schema input =
  case span (/= ' ') input of
    (cmd, args) => parseCommand schema cmd (ltrim args)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input =
  case parse (schema store) input of
    Left err =>
      Just ("Invalid command: " ++ err ++ "\n", store)
    Right (SetSchema newSchema) =>
      case setSchema store newSchema of
        Left err => Just (err ++ "\n", store)
        Right newStore => Just ("Schema changed.\n", newStore)
    Right (Add item) =>
      Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
    Right Print =>
      Just (
        foldl (\s, (idxFin, elem) =>
          s ++ show (finToNat idxFin) ++ ": " ++ showEntry elem ++ "\n"
        ) "Contents:\n" (zip range (items store)),
        store
      )
    Right (Get pos) =>
      case getEntry pos store of
        Just entry => Just (showEntry entry ++ "\n", store)
        Nothing => Just ("No such entry: " ++ show pos ++ "\n", store)
    Right Quit =>
      Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
