otp :: [Bool] -> [Bool] -> [Bool]
otp k m = map (\(x,y) -> x /= y) (zip k m)

-- 

isPrime n = [x | x <- [1..n], n `mod` x == 0] == [1, n]

primesTo m
	| m == 0 = []
	| isPrime m = m : primesTo (m-1)
	| otherwise = primesTo (m-1)

primes m = take m [p | p <- [2..], isPrime p]