-- assignment 3
improve :: Float -> Float -> Float
improve x y = (y + (x / y)) / 2

eps :: Float
eps = 0.001

goodEnough :: Float -> Float -> Bool
goodEnough x y = (abs (y^2 - x)) < eps

mySqrt :: Float -> Float
mySqrt x = calc x 1
  where
    calc x y
      | goodEnough x y = y
      | otherwise = calc x (improve x y)
