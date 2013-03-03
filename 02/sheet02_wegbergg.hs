{-
Student: Gregor Wegberg (wegbergg)
-}

import Debug.Trace

-- Assignment 3:
improv :: Float -> Float -> Float
improv x y = (y + (x / y)) / 2

goodEnough :: Float -> Float -> Bool
goodEnough x y = (abs ((y * y) - x)) < eps
    where
        eps = 0.001

mySqrt :: Float -> Float
mySqrt x = calcSqrt x initY
    where
        calcSqrt x y
            | goodEnough x y = y
            | otherwise = calcSqrt x (improv x y)
        initY = 1

-- Assignment 4:
coinsInit = [500, 200, 100, 50, 20, 10, 5]
-- coinsInit = [200, 100, 50, 20, 10, 5, 2, 1] -- used to check the code by solving Project Euler problem 31

cntChange :: Int -> Int
cntChange x = calcCnt x coinsInit 
    where
        calcCnt x coins
            -- | trace ("calc " ++ show x ++ " " ++ show coins) False = undefined
            | x > 0 && length coins > 0 = (calcCnt x (tail coins)) + (calcCnt (x - (head coins)) coins)
            | x == 0 = 1
            | otherwise = 0

