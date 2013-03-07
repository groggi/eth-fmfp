{-
Student: Gregor Wegberg, wegbergg
-}

-- Assignment 2 - Example message / key and helpers
exMsg = [False, True, True]
exKey = [False, False, True]

xor1 a b = (a || b) && not (a && b)
xor2 (a, b) = xor1 a b 

-- Assignment 2a - simpel solution with pattern matching
otpSimple :: [Bool] -> [Bool] -> [Bool]
otpSimple (m:ms) (k:ks) = (xor1 m k) : otpSimple ms ks
otpSimple [] [] = []

-- Assignment 2a - solution with map & zip
otpMapZip :: [Bool] -> [Bool] -> [Bool]
otpMapZip msg key = map xor2 (zip msg key)

-- Assignment 2b - using an other zip* function
optZipWith :: [Bool] -> [Bool] -> [Bool]
optZipWith msg key = zipWith xor1 msg key

-- Assignment 3a
isPrime a = [x | x <- [1 .. a], a `mod` x == 0] == [1, a]

-- Assignment 3b
listPrimes n = [x | x <- [1 .. n], isPrime x]

-- Assignment 3c
takePrimes n = findPrimes n 1
    where
        findPrimes n a
            | length (listPrimes (a * n)) < n = findPrimes n (2 * a)
            | otherwise = take n (listPrimes (a * n))
            
-- Assignment 4 - merge sort
{-mergeSort xs = merge left right
    where
        half xs = length xs `div` 2 -- old version: floor(length(xs) / 2)
        left = mergeSort (take (half xs) xs)
        right = mergeSort (drop (half xs) xs)
        
        merge [] [] = []
        merge [] xs = xs
        merge xs [] = xs
        merge (x:xs) (y:ys)
            | x < y     = x : merge xs (y:ys)
            | otherwise = y : merge (x:xs) ys-}
