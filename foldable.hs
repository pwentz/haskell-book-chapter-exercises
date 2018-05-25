import Data.Monoid

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap' id

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap' Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap' Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' needle haystack = foldr isMatch False haystack
  where
    isMatch elt acc
        | elt == needle = True
        | otherwise = acc

toList' :: (Foldable t) => t a -> [a]
toList' = foldMap' return

concatMap' :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap' = foldMap'

data Constant a b =
    Constant b

instance Foldable (Constant a) where
    foldMap f (Constant val) = f val

data Two a b =
    Two a
        b

instance Foldable (Two a) where
    foldMap f (Two _ val) = f val

data Three a b c =
    Three a
          b
          c

instance Foldable (Three a b) where
    foldMap f (Three _ _ val) = f val

data Three' a b =
    Three' a
           b
           b

instance Foldable (Three' a) where
    foldMap f (Three' _ val1 val2) = f val1 `mappend` f val2

data Four' a b =
    Four' a
          b
          b
          b

instance Foldable (Four' a) where
    foldMap f (Four' _ v1 v2 v3) = f v1 `mappend` f v2 `mappend` f v3

-- filterF' ::
--        (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF' pred = foldMap' emptyIfFalse
  where
    emptyIfFalse x
        | pred x = x
        | otherwise = mempty
