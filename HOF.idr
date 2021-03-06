identity: ty -> ty
identity x = x

add: Int -> Int -> Int
add x y = x + y

double: Num ty => ty -> ty
double x = x + x

twice: (a -> a) -> a -> a
twice f x = f (f x)

Shape: Type
rotate: Shape -> Shape

quadruple: Num a => a -> a
quadruple = twice double

turn_around: Shape -> Shape
turn_around = twice rotate
