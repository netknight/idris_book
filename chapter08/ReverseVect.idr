import Data.Vect


-- reverseProof: (x : elem) -> (xs : Vect len elem) -> (result: Vect (len + 1) elem) -> Vect (S len) elem
-- reverseProof {len} x xs result = rewrite plusCommutative 1 len in result

myReverse: Vect n elem -> Vect n elem
myReverse [] = []
{-
-- Won't compile
myReverse (x :: xs) = myReverse xs ++ [x]
-}
{-
-- With rewrite rule
myReverse {n = S len} (x :: xs) = let result = myReverse xs ++ [x] in
                                  rewrite plusCommutative 1 len in result
-}
myReverse (x :: xs) = reverseProof (myReverse xs ++ [x])
  where
    reverseProof: (result: Vect (len + 1) elem) -> Vect (S len) elem
    reverseProof {len} result = rewrite plusCommutative 1 len in result

append_nil: Vect m elem -> Vect (plus m 0) elem
append_nil {m} xs = rewrite plusZeroRightNeutral m in xs

append_xs: Vect (S (m + len)) elem -> Vect (plus m (S len)) elem
append_xs {m} {len} xs = rewrite sym (plusSuccRightSucc m len) in xs

-- m + n are switched their places to make task harder with commutative problem
append: Vect n elem -> Vect m elem -> Vect (m + n) elem
append [] ys = append_nil ys
append (x :: xs) ys = append_xs (x :: append xs ys)


plusComutes: (n: Nat) -> (m: Nat) -> n + m = m + n
plusComutes Z m = sym (plusZeroRightNeutral m)
plusComutes (S k) m = rewrite plusComutes k m in plusSuccRightSucc m k

myReverse2: Vect n elem -> Vect n elem
myReverse2 xs = reverse' [] xs
  where
    reverse': Vect n elem -> Vect m elem -> Vect (n + m) elem
    reverse' acc [] = append_nil acc
    reverse' acc (x :: xs) = append_xs (reverse' (x :: acc) xs)
