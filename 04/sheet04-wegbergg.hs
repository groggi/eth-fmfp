{-
Student: Gregor Wegberg, wegbergg
-}

-- Assignment 1
type Number = (Int, [Int])
toInt :: Number -> Int

toInt (base, digits) = sum (zipWith (*) baseEx digits) -- creates a list of base^i * digit_i and sums the list up at the end
    where
        baseEx = take (length digits) (iterate (base*) 1) -- creates a list of the form: [base^0, base^1, base^2, base^3, ...]
        
-- Assignment 3a
split :: Char -> String -> [String]

split _ [] = []
split d str = left : split d right
    where
        left = fst (nextPart)
        right = tail (snd nextPart)
        nextPart = span (/= d) str
        
-- Assignment 3b
center :: [String] -> [String]

center strs = map (centerStr (maximum $ map length strs)) strs
    where
        centerStr maxLen str = left ++ str ++ right
            where
                lenLeft = (maxLen - (length str)) `div` 2
                lenRight = maxLen - lenLeft - (length str)
                left = replicate lenLeft ' '
                right = replicate lenRight ' '
                
-- Assignment 4
filterMap' :: (b -> Bool) -> (a -> b) -> [a] -> [b]

-- filterMap p f = filter p . map f
filterMap' p f xs = foldr (\x xs -> if p (f x) then (f x) : xs else xs) [] xs
