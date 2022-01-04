import Data.List.Views
import Data.Vect
import Data.Vect.Views
import Data.Nat.Views

equalSuffix: Eq a => List a -> List a -> List a
equalSuffix input1 input2 with (snocList input1)
  equalSuffix [] input2 | Empty = []
  equalSuffix (xs ++ [x]) input2 | (Snoc xsrec) with (snocList input2)
    equalSuffix (xs ++ [x]) [] | (Snoc xsrec) | Empty = []
    equalSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xsrec) | (Snoc ysrec)
      = if x == y then (equalSuffix xs ys | xsrec | ysrec) ++ [x]
        else []
-- equalSuffix [1,2,4,5] [1..5]
-- [4,5]
-- equalSuffix [1,2,4,5,6] [1..5]
-- []
-- equalSuffix [1,2,4,5,6] [1..6]
-- [4,5,6]


mergeSort: Ord a => (input: Vect size a) -> Vect size a
mergeSort input with (splitRec input)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (lefts ++ rights) | (SplitRecPair lrec rrec) = merge (mergeSort lefts | lrec) (mergeSort rights | rrec)
-- mergeSort [5,3,4,1,2]

-- Hangs: Error in Idris 1.30. Cannot fully evaluate, need exec
toBinary: (input: Nat) -> String
toBinary input with (halfRec input)
  toBinary Z | HalfRecZ = ""
  toBinary (n + n) | (HalfRecEven rec) = (toBinary n | rec) ++ "0"
  toBinary (S (n + n)) | (HalfRecOdd rec) = (toBinary n | rec) ++ "1"
-- toBinary 42
-- 1010101
-- toBinary 94
-- 1011110
{--
42 / 2 = 21 -> 1
21 / 2 = 10 -> 0
10 / 2 = 5 -> 1
5 / 2 = 2 -> 0
2 / 2 = 1 -> 1
--}

palindrome : Eq a => List a -> Bool
palindrome xs with (vList xs)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (ys ++ [y])) | (VCons rec) = if x == y then palindrome ys | rec
                                                          else False
-- palindrome (unpack "abccba")
-- True
-- palindrome (unpack "abcba")
-- True
-- palindrome (unpack "abcb")
-- False
