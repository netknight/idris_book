data SplitList: List a -> Type where
  SplitNil: SplitList []
  SplitOne: SplitList [x]
  SplitPair: (lefts: List a) -> (rights: List a) -> SplitList (lefts ++ rights)

total
splitList: (input: List a) -> SplitList input
splitList input = splitListHelp input input
  where
    splitListHelp: List a -> (input: List a) -> SplitList input
    splitListHelp _ [] = SplitNil
    splitListHelp _ [x] = SplitOne
    splitListHelp (_ :: _ :: counter) (item :: items) = case splitListHelp counter items of
                                                             SplitNil => SplitOne
                                                             SplitOne {x} => SplitPair [item] [x]
                                                             SplitPair lefts rights => SplitPair (item :: lefts) rights
    splitListHelp _ items = SplitPair [] items

-- splitList [1,2,3,4,5]
-- splitListHelp (1::2::[3,4,5]) (1::[2,3,4,5]) -> SplitPair [1, 2] [3,4,5]
-- splitListHelp (3::4::[5]) (2::[3,4,5]) -> SplitPair [2] [3,4,5]
-- splitListHelp [5] [3,4,5] -> SplitPair [] [3,4,5]

-- splitList [1,2,3,4,5,6]
-- splitListHelp (1::2::[3,4,5,6]) (1::[2,3,4,5,6]) -> SplitPair [1,2,3] [4,5,6]
-- splitListHelp (3::4::[5,6]) (2::[3,4,5,6]) -> SplitPair [2,3] [4,5,6]
-- splitListHelp ([5,6::[]]) (3::[4,5,6]) -> SplitPair [3] [4, 5,6]
-- splitListHelp [] [4,5,6] -> SplitPair [] [4, 5,6]

-- splitList [1,2,3,4,5,6,7]
-- splitListHelp (1::2::[3,4,5,6,7]) (1::[2,3,4,5,6,7]) -> SplitPair [1, 2, 3] [4,5,6,7]
-- splitListHelp (3::4::[5,6,7]) (2::[3,4,5,6,7]) -> SplitPair [2, 3] [4,5,6,7]
-- splitListHelp (5::6::[7]) (3::[4,5,6,7]) -> SplitPair [3] [4,5,6,7]
-- splitListHelp [7] [4,5,6,7] -> SplitPair [] [4,5,6,7]

mergeSort: Ord a => List a -> List a
mergeSort input with (splitList input)
  mergeSort [] | SplitNil = []
  mergeSort [x] | SplitOne = [x]
  mergeSort (lefts ++ rights) | (SplitPair lefts rights) = merge (mergeSort lefts) (mergeSort rights)

-- mergeSort [5,1,4,3,2,6,8,7,9]
