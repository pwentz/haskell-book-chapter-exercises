import Control.Monad

-- List monad
--
data List a
    = Nil
    | Cons a
           (List a)

-- from applicatives/PropertyTesting
instance Monoid (List a) where
    mappend xs ys =
        case xs of
            Nil -> Nil
            Cons head rest -> Cons head $ rest `mappend` ys
    mempty = Nil

instance Functor List where
    fmap f list =
        case list of
            Nil -> Nil
            Cons x rest -> Cons (f x) (fmap f rest)

instance Applicative List where
    pure x = Cons x Nil
    f <*> x =
        case f of
            Nil -> Nil
            Cons headF restF -> fmap headF x `mappend` (restF <*> x)

instance Monad List where
    return = pure
    list >>= f = fold mappend Nil $ fmap f list

fold :: (a -> b -> b) -> b -> List a -> b
fold _ init Nil = init
fold f init (Cons x xs) = f x $ fold f init xs

-- Implement type sigs with monad and functor methods (NOT applicative)
join' :: Monad m => m (m a) -> m a
join' = (>>= id)

liftm1 :: Monad m => (a -> b) -> m a -> m b
liftm1 = fmap

liftm2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftm2 f m1 m2 = join $ fmap (<$> m2) (fmap f m1)

ap' :: Monad m => m (a -> b) -> m a -> m b
ap' fn m1 = join $ fmap (<$> m1) fn

{-| Alternative implementations:

    meh [] f = return []
    meh (x:xs) f = liftm2 (:) (f x) (meh xs f)
    -- or --
    meh (x:xs) f = (:) <$> (f x) <*> (meh xs f)
    -- or --
    meh (x:xs) f = fmap (:) (f x) <*> (meh xs f)
    -- or --
    mah (x:xs) f = pure (:) <*> (f x) <*> (meh xs f)

-}
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = foldr (liftm2 (:) . f) (return []) xs

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id
