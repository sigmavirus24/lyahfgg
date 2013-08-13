-- Poor name for a silly function
euclid x y =
    if x > y then
        (x `div` y, x `mod` y)
    else
        (y `div` x, y `mod` x)

-- I sort of liked pattern matching at first
gcd' 0 x = x
gcd' x 0 = x
gcd' _ 1 = 1
gcd' 1 _ = 1
-- After those 4 lines though, I'm disappointed that they're order dependent.
-- I was tempted to write if-then-else blocks instead of that since they
-- would probably have been simpler.
gcd' x y =
    if x < y then
        gcd' x (y `mod` x)
    else
        gcd' y (x `mod` y)

-- MUCH BETTER
gcd'' x y
    | x == 0 = y
    | x == 1 || y == 1 = 1
    | y == 0 = x
    | x < y = gcd'' x (y `mod` x)
    | otherwise = gcd'' y (x `mod` y)

-- Find the number of trailing zeros of a factorial
zeros_of_a_factorial x =
    if 0 <= x && x < 5 then
        0
    else
        sum [z | y <- [1..x], let z = x `div` (5^y), z > 0]
