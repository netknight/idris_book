import Data.Vect

fourInts: Vect 4 Int
fourInts = [0, 1, 2, 3]

sixInts: Vect 6 Int
sixInts = [4, 5, 6, 7, 8, 9]

tenInts: Vect 10 Int
tenInts = fourInts ++ sixInts

total wordLengths: Vect len String -> Vect len Nat
wordLengths [] = []
wordLengths (word :: words) = length word :: wordLengths words

insert: Ord elem => (x: elem) -> (xsSorted: Vect k elem) -> Vect (S k) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs

insSort: Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                        insert x xsSorted

myLength: List a -> Nat
myLength [] = 0
myLength (x :: xs) = length (x :: xs)

myReverse: List a -> List a
myReverse [] = []
myReverse (x :: xs) = reverse (x :: xs)

listMap: (a -> b) -> List a -> List b
listMap f [] = []
listMap f (x :: xs) = f x :: listMap f xs

vectMap: (a -> b) -> Vect n a -> Vect n b
vectMap f [] = []
vectMap f (x :: xs) = f x :: vectMap f xs

append: {elem: Type} -> {n: Nat} -> {m: Nat} ->
  Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] [] = []
append [] ys = ys
append (x :: xs) ys = x :: append xs ys
{-
append {elem = Char} {n = 2} {m = 3}
append : Vect 2 Char -> Vect 3 Char -> Vect 5 Char
-}

length: Vect n elem -> Nat
length {n} xs = n

myzip: Vect n a -> Vect n b -> Vect n (a, b)
myzip [] ys = []
myzip (x :: xs) (y :: ys) = (x, y) :: zip xs ys
