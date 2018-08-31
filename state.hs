{-# LANGUAGE InstanceSigs #-}

newtype State s a =
  State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State $ \s -> let (val, newState) = g s in (f val, newState)

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State ((,) a)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (State f) <*> (State g) =
    State $ \s ->
      let (val, newState) = g s
          (newFn, _) = f s
      in (newFn val, newState)

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State g) >>= f =
    State $ \s ->
      let (val, _) = g s
          (State getFinal) = f val
      in getFinal s

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put x = State $ \_ -> ((), x)

exec :: State s a -> s -> s
exec (State f) s = snd (f s)

eval :: State s a -> s -> a
eval (State f) s = fst (f s)

modify :: (s -> s) -> State s ()
modify f =
  State $ \s -> ((), f s)
  -- get >>= (put . f)
