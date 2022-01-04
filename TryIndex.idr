import Data.Vect

--index: Fin n -> Vect n a -> a

tryIndex: Integer -> Vect n a -> Maybe a
tryIndex {n} i xs = case integerToFin i n of
                         Nothing => Nothing
                         (Just x) => Just (index x xs)

vectTake: (n: Nat) -> Vect (n + m) a -> Vect n a
vectTake Z xs = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

{-
vTake: (Fin n) -> Vect (n + m) a -> Vect n a
vTake FZ xs = Nil
vTake (FS x) xs = ?vTake_rhs_2
-}

{-
maybeSum: Num a => Maybe a -> Maybe a -> Maybe a
maybeSum Nothing y = Nothing
maybeSum (Just x) y = ?maybeSum_rhs_2
-}

sumEntries: Num a => (pos: Integer) -> Vect n a -> Vect n a -> Maybe a
{-
sumEntries {n} pos xs ys = case (tryIndex pos xs, tryIndex pos ys) of
  (Just x, Just y) => Just (x + y)
  _ => Nothing
-}
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                (Just i) => Just (index i xs + index i ys)
