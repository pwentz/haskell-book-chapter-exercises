{-# LANGUAGE InstanceSigs #-}

import Control.Applicative
import Data.Maybe

newtype Reader r a = Reader
    { getReader :: (r -> a)
    }

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 fn x y = pure fn <*> x <*> y

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader (\r -> a)
    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    --(<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
    (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
    return = pure
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> (getReader $ aRb (ra r)) r

x :: [Integer]
x = [1 .. 3]

y :: [Integer]
y = [4 .. 6]

z :: [Integer]
z = [7 .. 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

{-| (,) :: a -> b -> (a, b)
    z' :: Integer -> Maybe Integer
    (,) . z' :: (Integer -> b -> (Maybe Integer, b))
    (,) . z' <*> :: (Integer -> (Maybe Integer, Maybe Integer))
    -- same as --
    \r -> (z' r, z' r)
-}
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) . z' <*> z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

{-| (&&) :: Bool -> Bool -> Bool
    (>3) :: Integer -> Bool
    (&&) . (>3) :: Integer -> Bool -> Bool
    (&&) . (>3) <*> (<8) :: Integer -> Bool
    -- same as --
    \r -> r > 3 && r < 8
-}
bolt :: Integer -> Bool
bolt = (&&) . (> 3) <*> (< 8)

main :: IO ()
main = do
    print $ foldr (&&) True (sequA 7)
    print $ (sequA . flip fromMaybe s') (-1)
    print $ (bolt . flip fromMaybe ys) (-1)
  where
    s' = summed <$> ((,) <$> xs <*> ys)

-- main :: IO ()
-- main = do
--     print $ sequenceA [Just 3, Just 2, Just 1]
--     print $ sequenceA [x, y]
--     print $ sequenceA [xs, ys]
--     print $ s'
--     print $ fmap summed ((,) <$> xs <*> zs)
--     print $ bolt 7
--     print $ fmap bolt z
--     print $ sequA 7
--   where
--     s' = summed <$> ((,) <$> xs <*> ys)
{-| Same as:

    map ($ m) [(> 3), (< 8), even]
-}
sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(> 3), (< 8), even] m
