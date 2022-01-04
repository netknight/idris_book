data Vect: (len: Nat) -> (elem: Type) -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

Eq elem => Eq (Vect n elem) where
  [] == [] = True
  (x :: xs) == (y :: ys) = case x == y of
                                False => False
                                True => xs == ys

Foldable (Vect n) where
  foldr f acc [] = acc
  foldr f acc (x :: xs) = f x (foldr f acc xs)
