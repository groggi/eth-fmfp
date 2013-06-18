-- assignment 2
otp :: [Bool] -> [Bool] -> [Bool]
otp k m = map xor (zip k m)
  where
    xor (a, b)
      | a && b = False
      | not a && not b = False
      | otherwise = True

otp2 :: [Bool] -> [Bool] -> [Bool]
otp2 k m = zipWith xor k m
  where
      xor a b
        | a && b = False
        | not a && not b = False
        | otherwise = True

-- assignment 3
isPrim p = [x | x <- [1..p], p `mod` x == 0] == [1, p]
primes m = [x | x <- [2..m], isPrim x]
firstPrimes m = take m (filter isPrim [1..])
