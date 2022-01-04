module Main

import Data.Vect

data DataStore: Type where
  mkData: (size: Nat) -> (items: Vect size String) -> DataStore

size: DataStore -> Nat
size (mkData size items) = size

items: (store: DataStore) -> Vect (size store) String
items (mkData size items) = items

addToStore: DataStore -> String -> DataStore
addToStore (mkData size items) item = mkData _ (addToData items)
  where
    addToData: Vect old String -> Vect (S old) String
    addToData [] = [item]
    addToData (x :: xs) = x :: addToData xs

data Command = Add String | Get Integer | Search String | Size | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" pos = case all isDigit (unpack pos) of
                             False => Nothing
                             True => Just (Get (cast pos))
parseCommand "search" str = Just (Search str)
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse: (input: String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                               case integerToFin pos (size store) of
                                     Nothing => Just ("Out of range\n", store)
                                     Just id => Just (index id store_items ++ "\n", store)

 --List (Nat, String)
--(len ** found) = filter (\v => isInfixOf v value) store_items in

findBySubstr: (value: String) -> (store: DataStore) -> List (Nat, String)
findBySubstr value store = let store_items = items store
                               indices = findIndices (\v => Strings.isInfixOf value v) store_items in
                               map (\v => (finToNat v, index v store_items)) indices

searchEntry : (value : String) -> (store : DataStore) -> Maybe (String, DataStore)
searchEntry value store = let found = findBySubstr value store in
                              Just ("Found:\n" ++ resToStr (found) ++ "\n", store)
                          where
                            resToStr: List (Nat, String) -> String
                            resToStr [] = ""
                            resToStr ((i, v) :: xs) = show (i) ++ ", " ++ v ++ "\n" ++ (resToStr xs)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
                                Nothing => Just ("Invalid command\n", store)
                                Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                                Just (Get pos) => getEntry pos store
                                Just (Search value) => searchEntry value store
                                Just Size => Just("Size " ++ show (size store) ++ "\n", store)
                                Just Quit => Nothing

main: IO ()
main = replWith (mkData _ []) "Command: " processInput
