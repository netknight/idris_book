data ListLast: List a -> Type where
  Empty: ListLast []
  NonEmpty: (xs: List a) -> (x: a) -> ListLast (xs ++ [x])

describeHelper: (input: List Int) -> (form: ListLast input) -> String
describeHelper [] Empty = "Empty"
describeHelper (xs ++ [x]) (NonEmpty xs x) = "Non-empty, initial = " ++ show xs

total
listLast: (xs: List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty => NonEmpty [] x
                          NonEmpty ys y => NonEmpty (x :: ys) y

describeListEnd: List Int -> String
describeListEnd xs = describeHelper xs (listLast xs)

describeListEnd_: List Int -> String
describeListEnd_ input with (listLast input)
  describeListEnd_ [] | Empty = "Empty"
  describeListEnd_ (xs ++ [x]) | (NonEmpty xs x) = "Non-empty, initial = " ++ show xs

-- Function fails :total check
myReverse: List a -> List a
myReverse input with (listLast input)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | (NonEmpty xs x) = x :: myReverse xs
