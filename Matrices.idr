import Data.Vect

createEmpties_: Vect n (Vect 0 elem)
createEmpties_ = replicate _ []

createEmpties: Vect n (Vect 0 elem)
createEmpties {n = Z} = []
createEmpties {n = (S k)} = [] :: createEmpties


{-
zipWith : (f : a -> b -> c) ->
  (xs : Vect n a) ->
  (ys : Vect n b) ->
  Vect n c
-}
transposeHelper : (x : Vect n elem) ->
  (xsTrans : Vect n (Vect k elem)) ->
  Vect n (Vect (S k) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

{-
[
[1, 2],
[3, 4], -> [[1, 3, 5],
[5, 6],    [2, 4, 6]]
]
-}
transposeMatrix: Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMatrix [] = createEmpties
transposeMatrix (x :: xs) = let xsTrans = transposeMatrix xs in
  transposeHelper x xsTrans

-- A task

transposeMat: Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = zipWith (::) x (transposeMat xs)

{-

1, 2   7  8    8  10
3, 4 + 9  10 = 12 14
5, 6   11 12   17 18

-}
addMatrix: Num numType =>
  Vect rows (Vect cols numType) ->
  Vect rows (Vect cols numType) ->
  Vect rows (Vect cols numType)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

h2: Num e => Vect n (Vect m e) -> Vect m e -> Vect n e
h2 [] _ = []
h2 (x :: xs) ys = sum (zipWith (*) x ys) :: h2 xs ys

h1: Num e => (m1 : Vect n (Vect m e)) -> (tm2 : Vect p (Vect m e)) -> Vect p (Vect n e)
h1 [] [] = []
h1 (x :: xs) [] = []
h1 [] (x :: xs) = [] :: h1 [] xs
h1 m (y :: ys) = h2 m y :: h1 m ys

{-
1, 2                    29,  32,  35,  38
3, 4 x 7,  8,  9,  10 = 65,  72,  79,  86
5, 6   11, 12, 13, 14   101, 112, 123, 134
-}
multMatrix: Num numType =>
  Vect n (Vect m numType) ->
  Vect m (Vect p numType) ->
  Vect n (Vect p numType)
multMatrix [] _ = []
multMatrix m1 m2 = let tm2 = transposeMat m2 in
  transposeMat (h1 m1 tm2)
