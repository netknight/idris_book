import Data.Vect

data VectUnknown: Type -> Type where
  MkVect: (len: Nat) -> Vect len a -> VectUnknown a

readVectLen: (len: Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do
  x <- getLine
  xs <- readVectLen k
  pure (x :: xs)

readVect: IO (VectUnknown String)
readVect = do
  x <- getLine
  if (x == "")
    then pure (MkVect _ [])
    else do
      MkVect _ xs <- readVect
      pure (MkVect _ (x:: xs))

printVect: Show a => VectUnknown a -> IO ()
printVect (MkVect len xs) = putStrLn (show xs ++ " (length " ++ show len ++ ")")

-- :exec readVect >>= printVect

-- Dependent pair is separated by **
anyVect: (n: Nat ** Vect n String) -- the type of n (Nat) can be infered from usage and not necessary to specify
anyVect = (3 ** ["1", "2", "3"])

readVect2: IO (len ** Vect len String)
readVect2 = do
  x <- getLine
  if (x == "")
    then pure (_ ** [])
    else do
      (_ ** xs) <- readVect2
      pure (_ ** x :: xs)

-- :exec readVect2 >>= printLn

zipInputs: IO ()
zipInputs = do
  putStrLn "Enter first vector (blank line to end):"
  (len1 ** vect1) <- readVect2
  putStrLn "Enter second vector (blank line to end):"
  (len2 ** vect2) <- readVect2
  case exactLength len1 vect2 of
    Nothing => putStrLn "Vectors have different lengths"
    Just vect2' => printLn (zip vect1 vect2')
