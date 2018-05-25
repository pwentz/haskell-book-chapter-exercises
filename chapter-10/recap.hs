-- Chapter 10: Folds
--
-- Recursive Patterns
-- Fold Right
--  - how foldr evaluates
--
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

{-|
    foldr' (+) 0 [1..5]
    -- (1+(2+(3+(4+(5+0)))))

    foldl (+) 0 [1..5]
    --(((((0+1)+2)+3)+4)+5)
-}

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

-- foldl' (+) (1 + 0) [2, 3, 4, 5]
-- foldl' (+) ((1 + 0) + 2)) [3, 4, 5]
-- foldl' (+) (((1 + 0) + 2) + 3) [4, 5]
-- foldl' (+) ((((1 + 0) + 2) + 3) + 4) [5]
-- foldl' (+) (((((1 + 0) + 2) + 3) + 4) + 5) []
-- (((((1 + 0) + 2) + 3) + 4) + 5) <- base case
-- ((((1 + 2) + 3) + 4) + 5)
-- (((3 + 3) + 4) + 5)
-- ((6 + 4) + 5)
-- (10 + 5)
-- 15
--
-- FOLDR
-- take 2 $ foldr (:) [] [1..3]
-- 1 : foldr (:) [] [2, 3]
-- 1 : (2 : foldr (:) [] [3])
-- 1 : (2 : (3 : foldr (:) [] []))
-- 1 : (2 : (3 : []))

-- FOLDL
-- foldl (flip (:)) [] [1..3]
-- foldl (flip (:)) (([] : 1) : 2) [3]
-- foldl (flip (:)) ((([] : 1) : 2) : 3) []
-- ((([] : 1) : 2) : 3)
-- (([1] : 2) : 3)
-- ([2, 1] : 3)
-- [3, 2, 1]



-- Evaluation
-- Foldr
-- take 2 $ foldr (:) [] [1..3]
-- take 2 (foldr (:) [] [1..3])
-- take 2 (1 : (foldr (:) [] [2, 3]))
-- 1 : (take 1 (foldr (:) [] [2, 3]))
-- 1 : (take 1 (2 : (foldr (:) [] [3])))
-- 1 : (2 : (take 0 (foldr (:) [] [3])))
-- (1 : (2 : []))
-- 1 : [2]
-- [1, 2]
take' :: Int -> [a] -> [a]
take' n [] = []
take' n (x:xs)
    | n == 0 = []
    | otherwise = x : take' (n - 1) xs

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f acc xs = foldr (\x acc' -> f x (head acc') : acc') [acc] xs
