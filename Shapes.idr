||| Represents shapes
data Shape =
  ||| A triangle with base length and height
  Triangle Double Double
  | ||| A rectangle with length and hieght
  Rectangle Double Double
  | ||| A circle with radius
  Circle Double

%name Shape shape, shape1, shape2

area: Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

-- recursive type
data Picture = Primitive Shape
  | Combine Picture Picture
  | Rotate Double Picture
  | Translate Double Double Picture

%name Picture pic, pic1, pic2

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture: Picture
testPicture = Combine (Translate 5 5 rectangle)
  (Combine (Translate 35 5 circle)
  (Translate 15 25 triangle))

pictureArea: Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

maxMaybe: Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just y) = Just y
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = Just (max x y)

biggestTriangle: Picture -> Maybe Double
biggestTriangle (Primitive shape) = case shape of
  ashape@(Triangle base height) => Just (area ashape)
  _ => Nothing
biggestTriangle (Combine pic pic1) = maxMaybe (biggestTriangle pic) (biggestTriangle pic1)
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

testPic1: Picture
testPic1 = Combine (Primitive (Triangle 2 3)) (Primitive (Triangle 2 4))

testPic2: Picture
testPic2 = Combine (Primitive (Rectangle 1 3)) (Primitive (Circle 4))
