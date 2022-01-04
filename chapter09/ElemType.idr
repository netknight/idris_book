data Vect: (len: Nat) -> (elem: Type) -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

data Elem: a -> Vect k a -> Type where
  Here: Elem x (x :: xs)
  There: (later: Elem x xs) -> Elem x (y :: xs)

data ListElem: a -> List a -> Type where
  ListHere: ListElem x (x :: xs)
  ListThere: (later: ListElem x xs) -> ListElem x (y :: xs)

notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There _) impossible

notInTail : (notThere : Elem value xs -> Void) -> (notHere : (value = x) -> Void) -> Elem value (x :: xs) -> Void
notInTail notThere notHere Here = notHere Refl
notInTail notThere notHere (There later) = notThere later

isElem: DecEq a => (value: a) -> (xs: Vect n a) -> Dec (Elem value xs)
isElem value [] = No notInNil
isElem value (x :: xs) = case decEq value x of
                             Yes Refl => Yes Here
                             No notHere => case isElem value xs of
                                                 Yes prf => Yes (There prf)
                                                 No notThere => No (notInTail notThere notHere)

elem: Eq ty => (value: ty) -> (xs: Vect n ty) -> Bool
elem value [] = False
elem value (x :: xs) = case value == x of
                            False => elem value xs
                            True => True

data Last: List a -> a -> Type where
  LastOne: Last [value] value
  LastCons: (prf: Last xs value) -> Last (x :: xs) value

notInListNil : Last [] value -> Void
notInListNil LastOne impossible
notInListNil (LastCons _) impossible

notInListHead : (notThere : (x = value) -> Void) -> Last [x] value -> Void
notInListHead notFirst LastOne = notFirst Refl
notInListHead _ (LastCons LastOne) impossible
notInListHead _ (LastCons (LastCons _)) impossible

notInListTail : (notLast : Last xs value -> Void) -> (notEmpty : (xs = []) -> Void) -> Last (x :: xs) value -> Void
notInListTail {value = x} notLast notEmpty LastOne = notEmpty Refl
notInListTail {value = value} notLast notEmpty (LastCons prf) = notLast prf

isLast: DecEq a => (xs: List a) -> (value: a) -> Dec (Last xs value)
isLast [] value = No notInListNil
isLast (x :: xs) value = case decEq xs [] of
                               Yes Refl => case decEq x value of
                                                Yes Refl => Yes LastOne
                                                No notFirst => No (notInListHead notFirst)
                               No notEmpty => case isLast xs value of
                                                   Yes prf => Yes (LastCons prf)
                                                   No notLast => No (notInListTail notLast notEmpty)


last123: Last [1,2,3] 3
