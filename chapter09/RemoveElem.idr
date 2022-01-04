import Data.Vect

removeElem: (value: a) -> (xs: Vect (S n) a) -> (prf: Elem value xs) -> Vect n a
removeElem x (x :: xs) Here = xs
removeElem {n = Z} value (x :: []) (There later) = absurd later
removeElem {n = (S k)} value (x :: xs) (There later) = x :: removeElem value xs later

--removeElem 2 [1,2,3,4,5] (There Here)
--$ removeElem 2 [1,2,3,4,5] (There Here)

removeElem_auto: (value: a) -> (xs: Vect (S n) a) -> {auto prf: Elem value xs} -> Vect n a
removeElem_auto value xs {prf} = removeElem value xs prf

--removeElem_auto 2 [1,2,3,4,5]
--$ removeElem_auto 2 [1,2,3,4,5]

--removeElem_auto 7 [1,2,3,4,5]
--$ (input):1:1-29:When checking argument prf to function Main.removeElem_auto:
--      Can't find a value of type
--            Elem 7 [1, 2, 3, 4, 5]

removeElem2: (value: a) -> (xs: Vect (S n) a) -> {auto prf: Elem value xs} -> Vect n a
removeElem2 x (x :: xs) {prf = Here} = xs
removeElem2 {n = Z} value (x :: []) {prf = There later} = absurd later
removeElem2 {n = (S k)} value (x :: xs) {prf = There later} = x :: removeElem2 value xs
