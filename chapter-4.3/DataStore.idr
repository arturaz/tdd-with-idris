module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size items) = size

items : (store : DataStore) -> Vect (size store) String
items (MkData size items) = items

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) str =
  MkData _ (addToData items)
  where
    addToData : Vect v_size String -> Vect (S v_size) String
    addToData [] = [str]
    addToData (x :: xs) = x :: (addToData xs)

data Command = Add String | Get Integer | Size | Search String | Print | Quit

parseInt : String -> Maybe Integer
parseInt input =
  if all isDigit (unpack input) then Just (cast input) else Nothing

parseCmd : String -> String -> Maybe Command
parseCmd "add" args = Just (Add args)
parseCmd "get" idxS = map (Get) (parseInt idxS)
parseCmd "size" "" = Just Size
parseCmd "search" str = Just (Search str)
parseCmd "print" "" = Just Print
parseCmd "quit" "" = Just Quit
parseCmd _ _ = Nothing

parse : String -> Maybe Command
parse input =
  case Strings.span (/= ' ') input of
    (cmd, args) => parseCmd cmd (ltrim args)

getEntry : (idx : Integer) -> (ds : DataStore) -> String
getEntry idx ds =
  let store_items = items ds
  in
    case integerToFin idx (size ds) of
      Nothing => "Out of range\n"
      Just id => "Entry: " ++ (Vect.index id store_items) ++ "\n"

search : (needle : String) -> (ds : DataStore) -> String
search needle ds =
  let ds_items = items ds
  in
    "'" ++ needle ++ "' " ++ (
      case findIndex (isInfixOf needle) ds_items of
        Nothing => "not Found"
        Just idx =>
          let idx_str = show (finToNat idx)
          in "found @ " ++ idx_str ++ ": " ++ (Vect.index idx ds_items)
    ) ++ "\n"

mkString : (sep : String) -> Vect n String -> String
mkString sep [] = ""
mkString sep (x1 :: x2 :: Nil) = x1 ++ sep ++ x2
mkString sep (x :: xs) = x ++ sep ++ (mkString sep xs)

printWithIndexes : Vect n String -> String
printWithIndexes {n} xs =
  let
    indexes = (Vect.range {len=n})
    zipped = zipWith (\item, idx => (show (finToNat idx)) ++ ": " ++ item) xs indexes
  in mkString "\n" zipped

print : (ds : DataStore) -> String
print (MkData size items) = printWithIndexes items

processCommand : (cmd : Command) -> (ds : DataStore) -> Maybe (String, DataStore)
processCommand (Add item) ds =
  Just ("ID " ++ show (size ds) ++ "\n", addToStore ds item)
processCommand (Get idx) ds =
  Just (getEntry idx ds, ds)
processCommand Size ds =
  Just ("Size: " ++ show (size ds) ++ "\n", ds)
processCommand (Search needle) ds =
  Just (search needle ds, ds)
processCommand Print ds =
  Just ((print ds) ++ "\n", ds)
processCommand Quit ds =
  Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput ds input =
  case parse input of
    Nothing => Just ("Invalid command\n", ds)
    Just cmd => processCommand cmd ds

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
