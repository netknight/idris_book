module Main

longerString: String -> String -> String
longerString x y = let xl = length x
                       yl = length y
                       res = if (xl < yl) then yl else xl in
                       show res


printLonger: String -> String -> IO ()
printLonger x y = putStrLn (longerString x y)

askAndPrintLonger: IO ()
askAndPrintLonger = putStrLn "Write 2 lines:" >>=
  \_ => getLine >>=
  \x => getLine >>=
  \y => (printLonger x y)

askAndPrintLongerWithDo: IO ()
askAndPrintLongerWithDo = do
  putStrLn "Write 2 lines:"
  x <- getLine
  y <- getLine
  printLonger x y

  
