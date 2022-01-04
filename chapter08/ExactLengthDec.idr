interface DecEq_ ty where
  decEq_: (val1: ty) -> (val2: ty) -> Dec (val1 = val2)

data Vect: (len: Nat) -> (elem: Type) -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

exactLength: (len: Nat) -> (input: Vect m elem) -> Maybe (Vect m elem)
exactLength {m} len input = case decEq m len of
                                 Yes prf => Just input
                                 No contra => Nothing

headUnequal: DecEq a => {xs: Vect n a} -> {ys: Vect n a}
  -> (contra: x = y -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

tailUnequal: DecEq a => {xs: Vect n a} -> {ys: Vect n a}
  -> (contra: (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl

DecEq a => DecEq (Vect n a) where
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys) = case decEq x y of
                                   -- $ - operator is for avoiding parentheses
                                   -- No (headUnequal contra)
                                   No contra => No $ headUnequal contra
                                   Yes Refl => case decEq xs ys of
                                                    No contra => No $ tailUnequal contra
                                                    Yes Refl => Yes Refl
-- test: decEq (the (Vect _ _) [1,2,3]) [1,2,3]
