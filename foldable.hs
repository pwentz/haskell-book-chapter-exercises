import Data.Semigroup (Product(..), Sum(..), Any(..))
import Data.Foldable
import Control.Applicative

data Id a =
  Id a
  deriving (Eq, Show)

data Optional a
  = Nil
  | Ok a
  deriving (Eq, Show)

data List a
  = Empty
  | a :|: List a
  deriving (Eq, Show)

infixr 5 :|:

instance Foldable Id where
  foldMap f (Id a) = f a
  foldr f acc (Id a) = f a acc

instance Foldable Optional where
  foldMap _ Nil = mempty
  foldMap f (Ok x) = f x
  foldr _ acc Nil = acc
  foldr f acc (Ok x) = f x acc

instance Foldable List where
    foldMap _ Empty = mempty
    foldMap f (x :|: xs) = f x `mappend` foldMap f xs
    foldr _ acc Empty = acc
    foldr f acc (x :|: xs) = f x (foldr f acc xs)
    foldl _ acc Empty = acc
    foldl f acc (x :|: xs) = foldl f (f acc x) xs


{-| CHAPTER EXERCISES
-}

sum' :: (Foldable t, Num a) => t a -> a
sum' =
  getSum . foldMap Sum
  -- foldr (+) 0

product' :: (Foldable t, Num a) => t a -> a
product' =
  getProduct . foldMap Product
  -- foldr (*) 1

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x =
  getAny . foldMap (Any . (== x))
  -- foldr ((||) . (== x)) False

newtype Min a = Min {getMin :: Maybe a}

instance Ord a => Monoid (Min a) where
    mempty = Min Nothing
    mappend x (Min Nothing) = x
    mappend (Min Nothing) x = x
    mappend (Min (Just x)) (Min (Just y)) = Min $ Just (min x y)

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' =
  getMin . foldMap (Min . Just)

newtype Max a = Max {getMax :: Maybe a}

instance Ord a => Monoid (Max a) where
    mempty = Max Nothing
    mappend x (Max Nothing) = x
    mappend (Max Nothing) x = x
    mappend (Max (Just x)) (Max (Just y)) = Max $ Just (max x y)

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' =
  getMax . foldMap (Max . Just)

toList' :: Foldable t => t a -> [a]
toList' =
  foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' =
  foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f =
  foldr (mappend . f) mempty

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


filterF' :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF' pred =
  foldMap (\x -> if pred x then pure x else mempty)
