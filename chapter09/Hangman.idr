import Data.Vect

data WordState: (guessRemaining: Nat) -> (letters: Nat) -> Type where
  MkWordState: (word: String) -> (missing: Vect letters Char) -> WordState guessRemaining letters

data Finished: Type where
  Lost: (game: WordState 0 (S letters)) -> Finished
  Won:  (game: WordState (S guesses) 0) -> Finished

data ValidInput: List Char -> Type where
  Letter: (c: Char) -> ValidInput [c]

isValidNil: ValidInput [] -> Void
isValidNil (Letter _) impossible

isValidTwo: ValidInput (x :: y :: xs) -> Void
isValidTwo (Letter _) impossible

isValidInput: (cs: List Char) -> Dec (ValidInput cs)
isValidInput [] = No isValidNil
isValidInput (x :: []) = Yes (Letter x)
isValidInput (x :: (y :: xs)) = No isValidTwo

isValidString: (s: String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput (unpack s)

-- Removes element checking that element exists in Vector
removeElem: (value: a) -> (xs: Vect (S n) a) -> {auto prf: Elem value xs} -> Vect n a
removeElem x (x :: xs) {prf = Here} = xs
removeElem {n = Z} value (x :: []) {prf = There later} = absurd later
removeElem {n = (S k)} value (x :: xs) {prf = There later} = x :: removeElem value xs

-- Reads single letter and validates user's input (retries if incorrect), returs result with validity proof
readGuess: IO (x ** ValidInput x)
readGuess = do
  putStr "Guess: "
  x <- getLine
  case isValidString (toUpper x) of
       Yes prf => pure (_ ** prf)
       No contra => do
         putStrLn "Invalid guess!"
         readGuess

processGuess: (letter: Char) -> WordState (S guesses) (S letters)
  -> Either (WordState guesses (S letters)) (WordState (S guesses) letters)
processGuess letter (MkWordState word missing) = case isElem letter missing of
                                                      Yes prf => Right (MkWordState word (removeElem letter missing))
                                                      No contra => Left (MkWordState word missing)

game: WordState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} state = do
  (_ ** Letter letter) <- readGuess
  case processGuess letter state of
        Left newState => do
          putStrLn "Wrong!"
          case guesses of
                Z => pure (Lost newState)
                S k => game newState
        Right newState => do
          putStrLn "Right!"
          case letters of
                Z => pure (Won newState)
                S k => game newState

main: IO ()
main = do
  result <- game {guesses=2} (MkWordState "Test" ['T', 'E', 'S'])
  case result of
    Lost (MkWordState word missing) => putStrLn ("You lose! The word was: " ++ word)
    Won state => putStrLn "You won!"
