{-
Student: Gregor Wegberg (wegbergg)
-}

--- provided functions
gcdiv :: Int -> Int -> Int
gcdiv x y                         
 | x == y      = x
 | y > x       = gcdiv x (y-x)
 | otherwise   = gcdiv (x-y) y

{-
Assignment 1a) negative values:
    - first argument negative:
        runs as long as the system has enough ressources. It keeps using th
        second guard in which function increases the second argument and ends
        up again using the second guard.
        
    - second argument negative: 
        runs as long as the system has enough ressources. It keeps using the
        "otherwise" guard in which function increases the first argument and
        ends up again in the third guard.
        
    - both negative and same value:
        stops and returns the value of the arguments (the 'x') because of the
        first guard
        
    - both negative and not the same value:
        runs as long as the system has enough ressources. The used guard depends
        on which argument is bigger.
-}

gcdInt :: Int -> Int -> Int
gcdInt x y = gcdiv (myAbs x) (myAbs y)
    where
        myAbs a	-- yay, isn't it awesome?
            | a < 0     = -a
            | otherwise = a

simplerGcdInt x y = gcdiv (abs x) (abs y)

{-
Assignment 1c)
    No surprise that 3.6 7.2 and 3.6 7.19999999 result in the same value
-}
gcdivf :: Float -> Float -> Float
gcdivf x y                         
 | x == y      = x
 | y > x       = gcdivf x (y-x)
 | otherwise   = gcdivf (x-y) y
 
{-
Assignment 2
-}
re :: (Float, Float) -> Float
re (r, i) = r

im :: (Float, Float) -> Float
im (r, i) = i

conj :: (Float, Float) -> (Float, Float)
conj (r, i) = (r, -i)

addC :: (Float, Float) -> (Float, Float) -> (Float, Float)
addC (r1, i1) (r2, i2) = (r1 + r2, i1 + i2)

multC :: (Float, Float) -> (Float, Float) -> (Float, Float)
multC (r1, i1) (r2, i2) = (r1 * r2 - i1 * i2, r1 * i2 + i1 * r2)

absC :: (Float, Float) -> Float
absC (r, i) = sqrt (r^2 + i^2)
