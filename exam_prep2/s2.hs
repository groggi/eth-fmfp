improve :: Float -> Float -> Float
improve x y = (y + (x / y)) / 2

eps :: Float
eps = 0.001

goodEnough :: Float -> Float -> Bool
goodEnough x y = (abs (y * y - x)) < eps

mySqrt x = aux x 1
	where
		aux x y
			| goodEnough x y = y
			| otherwise = aux x (improve x y)

coins :: [Int]
coins = [500, 200, 100, 50, 20, 10, 5]

cntChange :: Int -> Int
cntChange v = aux v coins
	where
		aux v c
			| v == 0 = 1
			| v < 0 = 0
			| null c = 0
			| otherwise = (aux v (tail c)) + (aux (v - head c) c)