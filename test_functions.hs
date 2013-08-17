add :: (Num a) => a -> a -> a
add x y = x + y

-- head pulls the first element off a list
test_head :: Bool
test_head =
    1 == (head [1..])

-- last pulls the last element off a list
test_last :: Bool
test_last =
    4 == (last [1..4])

-- init takes everything from a list except the last element
test_init :: Bool
test_init =
    [1..3] == (init [1..4])

-- tail takes everything after the first element of a list
test_tail :: Bool
test_tail =
    [2..4] == (tail [1..4])

test_composition :: Bool
test_composition =
    4 == x
    where
        composed = add . succ
        x = composed 1 2


test_function_application :: Bool
test_function_application =
    4 == (succ $ add 1 2)

test_foldl :: Bool
test_foldl =
    11 == x
    where
        fold_sum = foldl (+) 0
        x = fold_sum [1, 2, 3, 5]

test_foldl_again :: Bool
test_foldl_again =
    y == x
    where
        y = reverse [1..4]
        rev_list = foldl (\acc x -> x : acc) []
        x = rev_list [1..4]

test_foldl1 :: Bool
test_foldl1 =
    11 == x
    where
        x = foldl1 (+) [1, 2, 3, 5]

test_foldr :: Bool
test_foldr =
    [2, 3, 4, 5] == x
    where
        list_incr = foldr (\x acc -> (x + 1) : acc) []
        x = list_incr [1..4]

test_foldr1 :: Bool
test_foldr1 =
    10 == x
    where
        x = foldr1 (+) [1..4]
