module Main

import System

readNumber: IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

readNumbers: IO (Maybe (Nat, Nat))
readNumbers = do
  Just num1_ok <- readNumber | Nothing => pure Nothing
  Just num2_ok <- readNumber | Nothing => pure Nothing
  pure (Just (num1_ok, num2_ok))

countdown: (secs: Nat) -> IO ()
countdown Z = putStrLn "Lift Off!"
countdown (S secs) = do
  putStrLn (show (S secs))
  usleep 1000000
  countdown secs

countdowns: IO ()
countdowns = do
  putStr "Enter starting number: "
  Just startNum <- readNumber | Nothing => do
    putStrLn "Invalid input"
    countdowns
  countdown startNum
  putStr "Anither try (y/n)?"
  yn <- getLine
  if yn == "y" then countdowns else pure ()

printWithAttempt: Nat -> String -> IO ()
printWithAttempt attempt str = putStrLn (str ++ " (" ++ (show attempt) ++ ")")

guess: (target: Nat) -> (attempt: Nat) -> IO ()
guess target attempt = do
  putStr "Try to guess a number: "
  let printResult = printWithAttempt attempt
  Just v <- readNumber | Nothing => do
    printResult "Invalid input"
    guess target attempt
  case compare target v of
    LT => do
      printResult "Value must be lesser"
      guess target (attempt + 1)
    EQ => pure ()
    GT => do
      printResult "Value must be greater"
      guess target (attempt + 1)

main: IO ()
main = do
  t <- time -- retures current millis from 1970
  let x = mod t 100 -- bound value to be between 0 an 100
  guess (fromIntegerNat x) 1
