-- assignment 2
re :: (Float, Float) -> Float
re (r, _) = r

im :: (Float, Float) -> Float
im (_, i) = i

conj :: (Float, Float) -> (Float, Float)
conj (r, i) = (r, -i)

add :: (Float, Float) -> (Float, Float) -> (Float, Float)
add (ra, ia) (rb, ib) = (ra + rb, ia + ib)

mult :: (Float, Float) -> (Float, Float) -> (Float, Float)
mult (ra, ia) (rb, ib) = (ra * rb - ia * ib, ra * ib + ia * rb)

absS :: (Float, Float) -> Float
absS (r, i) = sqrt (r*r + i*i)
