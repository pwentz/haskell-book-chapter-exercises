myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem elem = foldr ((||) . (== elem)) False

myReverse :: [a] -> [a]
myReverse = foldr (flip (++) . return) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x:acc else acc) []

squish :: [[a]] -> [a]
squish = foldr go []
  where
    go [] acc = acc
    go (x:xs) acc = x : go xs acc

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr maxBy (last xs) (init xs)
  where
      maxBy y z
          | f y z == GT = y
          | otherwise = z


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr minBy x xs
  where
      minBy y z
          | f y z == LT = y
          | otherwise = z
