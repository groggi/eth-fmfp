-- assignment 1
type Number = (Int, [Int])

toInt :: Number -> Int
toInt (base, digits) = sum (zipWith (*) digits (iterate (* base) 1))

