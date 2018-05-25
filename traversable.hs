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

{-| OPTIONALS
-}
data Optional a
    = None
    | Has a
    deriving (Eq, Show)

instance Functor Optional where
    fmap f None = None
    fmap f (Has x) = Has $ f x

instance Foldable Optional where
    foldMap f None = mempty
    foldMap f (Has x) = f x

instance Traversable Optional where
    traverse f opts =
        case fmap f opts of
            None -> pure None
            Has x -> fmap Has x

{-| LISTS
-}
data List a
    = Nil
    | Cons a
           (List a)
    deriving (Eq, Show)

instance Monoid (List a) where
    mempty = Nil
    mappend xs ys =
        case xs of
            Nil -> ys
            Cons head rest -> Cons head (rest `mappend` ys)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons head rest) = Cons (f head) $ fmap f rest

instance Foldable List where
    foldMap f Nil = mempty
    foldMap f (Cons head rest) = f head `mappend` foldMap f rest

instance Traversable List where
    traverse f = foldr (liftA2 Cons . f) (pure Nil)
    sequence = foldr (liftA2 Cons) (pure Nil)

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

{-| TREE!
-}
data Tree a
    = Empty
    | Leaf a
    | Node (Tree a)
           a
           (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node left root right) = Node (fmap f left) (f root) (fmap f right)

instance Foldable Tree where
    foldMap f tree =
        case tree of
            Empty -> mempty
            Leaf a -> f a
            Node left root right ->
                mconcat [foldMap f left, f root, foldMap f right]

instance Traversable Tree where
    traverse f = foldr (liftA2 (Node Empty) . f) (pure Empty)
