import Data.Vect

readToBlank : IO (List String)
readToBlank =
  do
  line <- getLine
  if line == ""
    then pure []
    else do lines <- readToBlank
            pure (line :: lines)

readAndSave : IO ()
readAndSave =
  do
  putStrLn "Enter lines (blank line to end):"
  lines <- readToBlank
  putStr "Enter filename to save to: "
  filename <- getLine
  let contents =
    if isNil lines then "" else foldl1 (\a, b => a ++ "\n" ++ b) lines
  result <- writeFile filename contents
  case result of
    -- file gets written, but an error is reported. Bah.
    Left err => putStrLn ("Can't write to file " ++ filename ++ ": " ++ show err)
    Right _ => putStrLn "File written."

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename =
  do
    Right file <- openFile filename Read
      | Left err => error ("Can't open file " ++ filename ++ ": " ++ show err )
    vector <- readFromFile file
    closeFile file
    pure vector
  where
    error : String -> IO (n ** Vect n String)
    error err =
      do
      putStrLn err
      pure (_ ** [])

    readFromFile : (file : File) -> IO (n ** Vect n String)
    readFromFile file =
      do
      False <- fEOF file
        | True => pure (_ ** [])
      Right line <- fGetLine file
        | Left err => error ("Can't read line from " ++ filename ++ ": " ++ show err)
      (_ ** lines) <- readFromFile file
      pure (_ ** (trim line :: lines))
