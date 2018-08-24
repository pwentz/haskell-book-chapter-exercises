{-# LANGUAGE InstanceSigs #-}

import Control.Applicative

data Id a = Id a
  deriving (Eq, Show)

instance Foldable Id where
  foldMap :: Monoid m => (a -> m) -> Id a -> m
  foldMap f (Id x) = f x

  foldr :: (a -> b -> b) -> b -> Id a -> b
  foldr f acc (Id a) = f a acc

instance Functor Id where
  fmap f (Id x) = Id (f x)

instance Traversable Id where
  traverse :: (Applicative g) => (a -> g b) -> Id a -> g (Id b)
  traverse f (Id x) = Id <$> f x

data Optional a = Nil | Ok a deriving (Eq, Show)

instance Foldable Optional where
  foldMap :: Monoid m => (a -> m) -> Optional a -> m
  foldMap _ Nil = mempty
  foldMap f (Ok x) = f x
  foldr :: (a -> b -> b) -> b -> Optional a -> b
  foldr _ acc Nil = acc
  foldr f acc (Ok x) = f x acc

instance Functor Optional where
  fmap _ Nil = Nil
  fmap f (Ok x) = Ok (f x)

instance Traversable Optional where
  traverse :: (Applicative g) => (a -> g b) -> Optional a -> g (Optional b)
  traverse _ Nil = pure Nil
  traverse f (Ok x) = Ok <$> f x

data List a = Empty | a :|: List a
    deriving (Eq, Show)

infixr 5 :|:

instance Foldable List where
  foldMap :: Monoid m => (a -> m) -> List a -> m
  foldMap _ Empty = mempty
  foldMap f (x :|: xs) = f x `mappend` foldMap f xs
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr _ acc Empty = acc
  foldr f acc (x :|: xs) = f x (foldr f acc xs)
  foldl :: (b -> a -> b) -> b -> List a -> b
  foldl _ acc Empty = acc
  -- foldl f acc (x :|: xs) = f (foldl f acc xs) x
  foldl f acc (x :|: xs) = foldl f (f acc x) xs

instance Functor List where
  fmap f Empty = Empty
  fmap f (x :|: xs) = f x :|: fmap f xs

instance Traversable List where
  traverse :: (Applicative g) => (a -> g b) -> List a -> g (List b)
  traverse _ Empty = pure Empty
  traverse f (x :|: xs) = liftA2 (:|:) (f x) (traverse f xs)
