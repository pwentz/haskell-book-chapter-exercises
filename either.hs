lefts' :: [Either a b] -> [a]
lefts' eithers = foldr keepLeft [] eithers
  where
    keepLeft either acc =
        case either of
            Left x -> x : acc
            Right _ -> acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' eithers = foldr splitEither ([], []) eithers
  where
    splitEither (Left l) (lefts, rights) = (l : lefts, rights)
    splitEither (Right r) (lefts, rights) = (lefts, r : rights)

mapLeft :: (a -> b) -> Either a x -> Either b x
mapLeft f (Right r) = Right r
mapLeft f (Left l) = Left (f l)

mapRight :: (a -> b) -> Either x a -> Either x b
mapRight f either =
    case either of
        Left l -> Left l
        Right r -> Right (f r)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' _ fn (Right r) = fn r
either' fn _ (Left l) = fn l

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
