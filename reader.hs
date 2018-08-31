{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

newtype Reader r a = Reader
  { runReader :: r -> a
  }

instance Functor (Reader r) where
  fmap :: (b -> c) -> Reader a b -> Reader a c
  fmap f (Reader ra) = Reader (\r -> f $ ra r)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure x =
    Reader (const x)

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> r `rab` (ra r)

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb $ ra r) $ r

x :: [Integer]
x = [1..3]

y :: [Integer]
y = [4..6]

z :: [Integer]
z = [7..9]
