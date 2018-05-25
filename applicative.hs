import Data.Monoid

data List a
    = Nil
    | Cons a
           (List a)
    deriving (Eq, Show)

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

take' :: Int -> List a -> List a
take' n Nil = Nil
take' n (Cons x xs)
    | n <= 0 = Nil
    | otherwise = (Cons x Nil) `mappend` (take' (n - 1) xs)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ init Nil = init
fold f init (Cons x xs) = f x (fold f init xs)

concat' :: List (List a) -> List a
concat' = fold mappend Nil

flatMap' :: (a -> List b) -> List a -> List b
flatMap' f list = concat' $ fmap f list

-- ZIPLIST
data ZList a =
    ZList (List a)
    deriving (Eq, Show)

instance Monoid (ZList a) where
    mappend (ZList xs) (ZList ys) = ZList $ mappend xs ys
    mempty = ZList Nil

instance Functor ZList where
    fmap f (ZList list) = ZList (fmap f list)

instance Applicative ZList where
    pure l = ZList (pure l)
    (ZList f) <*> (ZList list) =
        case f of
            Nil -> ZList Nil
            Cons headF tailF ->
                case list of
                    Nil -> ZList Nil
                    Cons head tail ->
                        ZList (pure $ headF head) `mappend`
                        (ZList tailF <*> ZList tail)

instance Eq a => EqProp (ZList a) where
    xs =-= ys = xs' `eq` ys'
      where
        xs' =
            let (ZList l) = xs
            in take' 3000 l
        ys' =
            let (ZList l) = ys
            in take' 3000 l

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' f l1 l2 =
    case l1 of
        Nil -> Nil
        Cons x xs ->
            case l2 of
                Nil -> Nil
                Cons y ys -> Cons (f x y) (zipWith' f xs ys)

-- VALIDATION
data Validation e a
    = Failure' e
    | Success' a
    deriving (Eq, Show, Generic)

instance Functor (Validation e) where
    fmap f v =
        case v of
            Failure' x -> Failure' x
            Success' x -> Success' (f x)

instance Monoid e => Applicative (Validation e) where
    pure = Success'
    f <*> v =
        case f of
            Failure' x ->
                case v of
                    Failure' x' -> Failure' (mappend x x')
                    _ -> Failure' x
            Success' fn -> fmap fn v

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq

{-| Monad instance of Validation CANNOT mappend together failure values.

    Why?
      - the second arg of >>= (the fn) relies on the first arg (the Monad) to
        be present
      - if the Monad is an identity value, then the function will simply not get run
      - the applicative instance could mappend any failures because the failures happen
        independently of each other
      - the Monad instance depends on the value within the non-identity part of the data structure,
        so it MUST exit early if the identity value is present
-}
instance Monoid e => Monad (Validation e) where
    return = pure
    res >>= f =
        case res of
            Failure' e -> Failure' e
            Success' val -> f val
