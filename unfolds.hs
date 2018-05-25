myIterate :: (a -> a) -> a -> [a]
myIterate fn val = val : myIterate fn (fn val)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = maybe [] recurse (f x)
  where
    recurse (y, x') = y : myUnfoldr f x'
