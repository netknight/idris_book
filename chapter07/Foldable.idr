summed: Nat
summed = foldr (+) 0 [1,2,3,4]

multiplied: Nat
multiplied = foldr (*) 1 [1,2,3,4]

totalLen: List String -> Nat
totalLen xs = foldr (\v, acc => length v + acc) 0 xs

interface Foldable' (t: Type -> Type) where
  foldr': (elem -> acc -> acc) -> acc -> t elem -> acc
  foldl': (acc -> elem -> acc) -> acc -> t elem -> acc

Foldable' List where
  foldr' f acc [] = acc
  foldr' f acc (x :: xs) = f x (foldr' f acc xs)

  foldl' f acc [] = acc
  foldl' f acc (x :: xs) = foldl' f (f acc x) xs
