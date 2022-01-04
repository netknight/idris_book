import Data.Vect

Position: Type
Position = (Double, Double)

Polygon: Nat -> Type
Polygon n = Vect n Position

triangle: Polygon 3
triangle = [(0, 0), (3.0, 0.0), (0.0, 4.0)]

Matrix: Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Double)

testMatrix: Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]
