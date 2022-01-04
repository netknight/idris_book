module Lists

l: List String
l = ["Quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"]

ls: String
ls = unwords l

ll: List String
ll = words ls

-- Filter all that is not > 10
filterGreater10: List Int -> List Int
filterGreater10 list = filter (> 10) list

{-
could be imported with import in other file
average function documentation could be viewed in REPL using :doc average
-}
||| Calculates the average length of words in a string.
||| @str a string containing words separated by whitespace.
export
average: (str: String) -> Double
average str = let numWords = wordCount str
                  totalLength = sum (wordLenghts (words str)) in
                  cast totalLength / cast numWords
  where
    wordCount: String -> Nat
    wordCount str = length (words str)

    wordLenghts: List String -> List Nat
    wordLenghts strs = map length strs

top_ten: Ord a => List a -> List a
top_ten lst = take 10 (reverse (sort lst))

over_length: Nat -> List String -> Nat
over_length l lst =  length (filter (\v => length v > l) lst)

-- Rewritten function with pattern matching and recursion
wordLengths: List String -> List Nat
wordLengths [] = []
wordLengths (word :: words) = length word :: wordLengths words
