{-# LANGUAGE InstanceSigs #-}

import Control.Applicative
import Data.Traversable

{-| IDENTITY
 -}
newtype Identity a =
    Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
    foldMap f (Identity x) = f x

instance Traversable Identity where
    traverse f (Identity x) = Identity <$> f x

{-| CONSTANT
 -}
newtype Constant a b = Constant
    { getConstant :: a
    }

instance Functor (Constant a) where
    fmap _ (Constant x) = Constant x

instance Monoid m => Monoid (Constant m b) where
    mempty = Constant mempty
    (Constant x) `mappend` (Constant y) = Constant (x `mappend` y)

instance Foldable (Constant a) where
    foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
    traverse f (Constant x) = pure $ Constant x

{-| OPTIONAL
-}

data Optional a
  = Nil
  | Ok a
  deriving (Show, Eq)

instance Functor Optional where
  fmap f Nil = Nil
  fmap f (Ok x) = Ok (f x)

instance Foldable Optional where
  foldMap _ Nil = mempty
  foldMap f (Ok x) = f x

instance Applicative Optional where
  pure = Ok
  (<*>) :: Optional (a -> b) -> Optional a -> Optional b
  Nil <*> _ = Nil
  (Ok f) <*> x = f <$> x

instance Traversable Optional where
  traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
  traverse _ Nil = pure Nil
  traverse f (Ok x) = Ok <$> f x

{-| LIST
-}

data List a
  = Empty
  | a :|: List a
  deriving (Show, Eq)

infixr 5 :|:

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap f Empty = Empty
  fmap f (x :|: xs) = f x :|: fmap f xs

instance Foldable List where
  foldMap :: Monoid m => (a -> m) -> List a -> m
  foldMap _ Empty = mempty
  foldMap f (x :|: xs) = f x `mappend` foldMap f xs

instance Traversable List where
  traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse _ Empty = pure Empty
  traverse f (x :|: xs) = liftA2 (:|:) (f x) (traverse f xs)

{-| Three arg constructors
-}
data Three a b c =
    Three a
          b
          c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
    traverse f (Three a b c) = fmap (Three a b) (f c)

{-| Pair
 -}
data Pair a b =
    Pair a
         b
    deriving (Eq, Show)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
    foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
    traverse f (Pair a b) = fmap (Pair a) (f b)

{-| Multiple values (same type)
-}
data Big a b =
    Big a
        b
        b
    deriving (Eq, Show)

instance Functor (Big a) where
    fmap f (Big a x x') = Big a (f x) (f x')

instance Foldable (Big a) where
    foldMap f (Big a x x') = f x `mappend` f x'

instance Traversable (Big a) where
    traverse f (Big a x x') = liftA2 (Big a) (f x) (f x')

data Bigger a b =
    Bigger a
           b
           b
           b
    deriving (Eq, Show)

instance Functor (Bigger a) where
    fmap f (Bigger a x x' x'') = Bigger a (f x) (f x') (f x'')

instance Foldable (Bigger a) where
    foldMap f (Bigger a x x' x'') = mconcat $ fmap f [x, x', x'']

instance Traversable (Bigger a) where
    traverse f (Bigger a x x' x'') = liftA3 (Bigger a) (f x) (f x') (f x'')

{-| TREE
-}

data Tree a
  = EmptyTree
  | Leaf a
  | Node (Tree a)
         a
         (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node left root right) = Node (fmap f left) (f root) (fmap f right)

instance Foldable Tree where
  foldMap f EmptyTree = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node left root right) = foldMap f left `mappend` f root `mappend` foldMap f right

instance Traversable Tree where
  traverse f EmptyTree = pure EmptyTree
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node left root right) = liftA3 Node (traverse f left) (f root) (traverse f right)
