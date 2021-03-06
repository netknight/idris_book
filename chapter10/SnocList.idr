data SnocList: List a -> Type where
  Empty: SnocList []
  Snoc: (rec: SnocList xs) -> SnocList (xs ++ [x])

snocListHelp: (snoc: SnocList input) -> (rest: List a) -> SnocList (input ++ rest)
snocListHelp {input = input} snoc [] = rewrite appendNilRightNeutral input in snoc
snocListHelp {input = input} snoc (x :: xs) = rewrite appendAssociative input [x] xs in snocListHelp (Snoc snoc {x}) xs

snocList: (xs: List a) -> SnocList xs
snocList xs = snocListHelp Empty xs

myReverseHelper: (input: List a) -> SnocList input -> List a
myReverseHelper [] Empty = []
myReverseHelper (xs ++ [x]) (Snoc rec) = x :: myReverseHelper xs rec

myReverse: List a -> List a
myReverse input = myReverseHelper input (snocList input)

myReverse_: List a -> List a
myReverse_ input with (snocList input)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | (Snoc rec) = x :: myReverse xs

isSuffix: Eq a => List a -> List a -> Bool
isSuffix input1 input2 with (snocList input1)
  isSuffix [] input2 | Empty = True
  isSuffix (xs ++ [x]) input2 | (Snoc xsrec) with (snocList input2)
    isSuffix (xs ++ [x]) [] | (Snoc xsrec) | Empty = False
    isSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xsrec) | (Snoc ysrec)
      = if x == y then isSuffix xs ys | xsrec  | ysrec
                  else False
-- isSuffix [7,8,9] [1..10]
-- False
-- isSuffix [7,8,9,10] [1..10]
-- True
