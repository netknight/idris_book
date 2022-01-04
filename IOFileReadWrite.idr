import Data.Vect

readToBlank: IO (List String)
readToBlank = do
  x <- getLine
  if x == ""
    then pure []
    else do
      xs <- readToBlank
      pure (x :: xs)

readAndSave: IO ()
readAndSave = do
  putStrLn "Write your text (blank line to end):"
  buffer <- readToBlank
  putStr "Write a filename: "
  filename <- getLine
  if (filename == "")
    then putStrLn "Invalid filename"
    else do
      let fname = filename ++ ".txt"
      putStrLn ("openedFile: " ++ fname)
      Right _ <- writeFile fname (unlines buffer) | Left err => putStrLn (show err)
      putStrLn "File written"

removeEOL: String -> String
removeEOL x = let r = reverse x in
                  if isPrefixOf "\n" r
                    then reverse (strTail r)
                    else x

readToEOF: (f: File) -> IO (len ** Vect len String)
readToEOF f = do
  eof <- fEOF f
  if (eof)
    then pure (_ ** [])
    else do
      Right line <- fGetLine f | Left _ => pure (_ ** [])
      (_ ** xs) <- readToEOF f
      let x = removeEOL line
      if x == ""
        then pure (_ ** xs)
        else pure (_ ** x :: xs)

readVectFile: (filename: String) -> IO (n ** Vect n String)
readVectFile filename = do
  Right h <- openFile filename Read | Left _ => pure (_ ** [])
  vect <- readToEOF h
  closeFile h
  pure vect
