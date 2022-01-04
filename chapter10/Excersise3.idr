import DataStore
import Shape_abs

testStore: DataStore (SString .+. SInt)
testStore = addToStore ("First", 1) $
            addToStore ("Second", 2) $
            addToStore ("Third", 3) $
            empty

getValues: DataStore (SString .+. val_schema) -> List (SchemaType val_schema)
getValues input with (storeView input)
  getValues x | SNil = []
  getValues (addToStore (key, value) store) | (SAdd rec) = value :: getValues store | rec
-- getValues testStore

area: Shape -> Double
area shape with (shapeView shape)
  area (triangle base height) | STriange = 0.5 * base * height
  area (rectangle width height) | SRectangle = width * height
  area (circle radius) | SCircle = pi * radius * radius
-- area (triangle 3 4)
-- 6.0
-- area (circle 10)
-- 314.xxx
