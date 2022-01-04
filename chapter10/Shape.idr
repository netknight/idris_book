module Shape

export
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

private
rectangle_area: Double -> Double -> Double
rectangle_area width height = width * height

export
area: Shape -> Double
area (Triangle base height) = 0.5 * rectangle_area base height
area (Rectangle base height) = rectangle_area base height
area (Circle radius) = pi * radius * radius
