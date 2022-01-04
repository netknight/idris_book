module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
  | SChar
  | SInt
  | (.+.) Schema Schema


SchemaType: Schema -> Type
SchemaType SString = String
SchemaType SChar = Char
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor mkData
  schema: Schema
  size: Nat
  items: Vect size (SchemaType schema)


addToStore: (store: DataStore) -> SchemaType (schema store) -> DataStore
addToStore (mkData schema size store) newItem = mkData schema _ (addToData store)
  where
    addToData: Vect oldSize (SchemaType schema) -> Vect (S oldSize) (SchemaType schema)
    addToData [] = [newItem]
    addToData (item :: items) = item :: addToData items

display: SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SChar} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (item1, item2) = display item1 ++ ", " ++ display item2

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                               case integerToFin pos (size store) of
                                    Nothing => Just ("Out of range\n", store)
                                    Just id => Just (display (index id (items store)) ++ "\n", store)

getAllEntries: (store : DataStore) -> (String, DataStore)
getAllEntries store = (displayItems (items store), store) where --unlines (map (\item => ?value) items)
  displayItems: (items: Vect size (SchemaType schema)) -> String
  displayItems [] = ""
  displayItems (x :: xs) = display x ++ "\n" ++ displayItems xs

setSchema: (store: DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (mkData schema _ [])
                              S k => Nothing

parseSchema: List String -> Maybe Schema
parseSchema ("String" :: xs) = case xs of
                                    [] => Just SString
                                    _ => map (\xs' => (SString .+. xs')) (parseSchema xs)
parseSchema ("Char" :: xs) = case xs of
                                  [] => Just SChar
                                  _ => map (\xs' => (SChar .+. xs')) (parseSchema xs)
parseSchema ("Int" :: xs) = case xs of
                                 [] => Just SInt
                                 _ =>  map (\xs' => (SInt .+. xs')) (parseSchema xs)
parseSchema _ = Nothing

parsePrefix: (schema: Schema) -> (input: String) -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted: List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = case span (/= '"') xs of
                                 (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                 _ => Nothing
    getQuoted _ = Nothing
parsePrefix SChar input = let (x :: xs) = unpack input in
                              Just (x, ltrim(pack xs))
parsePrefix SInt input = case span isDigit input of
                              ("", rest) => Nothing
                              (num, rest) => Just (cast num, ltrim rest)
parsePrefix (schema1 .+. schema2) input = do
  (val1, input') <- parsePrefix schema1 input
  (val2, input'') <- parsePrefix schema2 input'
  Just ((val1, val2), input'')

parseBySchema: (schema: Schema) -> (input: String) -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  (Just (res, "")) => Just res
                                  (Just _) => Nothing
                                  Nothing => Nothing


data Command: Schema -> Type where
  SetSchema: (newSchema: Schema) -> Command schema
  Add: SchemaType schema -> Command schema
  Get: Integer -> Command schema
  GetAll: Command schema
  Quit: Command schema

parseCommand : (schema: Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" str = map (\v => Add v) (parseBySchema schema str)
parseCommand schema "get" "" = Just (GetAll)
parseCommand schema "get" pos = case all isDigit (unpack pos) of
                                     False => Nothing
                                     True => Just (Get (cast pos))
parseCommand schema "schema" str = do
  validSchema <- parseSchema (words str)
  Just (SetSchema validSchema)
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

parse: (schema: Schema) -> (input: String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)

processInput : (store: DataStore) -> (input: String) -> Maybe (String, DataStore)
processInput store input = case parse (schema store) input of
                                Nothing => Just ("Invalid command\n", store)
                                Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                                Just (Get pos) => getEntry pos store
                                Just GetAll => Just (getAllEntries store)
                                Just (SetSchema inputSchema) => case setSchema store inputSchema of
                                                                     Nothing => Just ("Can't update schema\n", store)
                                                                     Just store' => Just ("OK\n", store')
                                Just Quit => Nothing

main: IO ()
main = replWith (mkData (SString .+. SString .+. SInt) _ []) "Command: " processInput
