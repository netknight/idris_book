import Data.List.Views

mergeSort: Ord a => List a -> List a
mergeSort input with (splitRec input)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (lefts ++ rights) | (SplitRecPair lrec rrec) = merge (mergeSort lefts | lrec) (mergeSort rights | rrec)
-- mergeSort [3,2,1]
-- mergeSort [5,1,4,3,2,6,8,7,9]
