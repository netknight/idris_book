data Vect: (len: Nat) -> (elem: Type) -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

data EqNat: (num1: Nat) -> (num2: Nat) -> Type where
  Same: (num: Nat) -> EqNat num num

sameS : (k: Nat) -> (j: Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS j j (Same j) = Same (S j)

data ThreeEq: (num1: Nat) -> (num2: Nat) -> (num3: Nat) -> Type where
  AllSame: (num: Nat) -> ThreeEq num num num

allSameS: (x, y, z: Nat) -> (eq: ThreeEq x y z) -> ThreeEq (S x) (S y) (S z)
allSameS z z z (AllSame z) = AllSame (S z)

checkEqNat: (num1: Nat) -> (num2: Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same 0)
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              Just eq => Just (sameS _ _ eq)

exactLength: (len: Nat) -> (input: Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat m len of
                                 Nothing => Nothing
                                 Just (Same len) => Just input

-- Generic equality type like EqNat built-in (can't compile due to reserved word)
{-
data (=): a -> b -> Type where
  Refl: x = x
-}

checkEqNat2: (num1: Nat) -> (num2: Nat) -> Maybe (num1 = num2)
checkEqNat2 Z Z = Just Refl
checkEqNat2 Z (S k) = Nothing
checkEqNat2 (S k) Z = Nothing
checkEqNat2 (S k) (S j) = case checkEqNat2 k j of
                              Nothing => Nothing
                              Just prf => Just (cong prf)

same_cons : {xs: List a} -> {ys: List a} -> xs = ys -> x :: xs = x :: ys
same_cons prf = cong prf

same_lists: {xs: List a} -> {ys: List a} -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl Refl = Refl
