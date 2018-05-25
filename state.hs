{-# LANGUAGE InstanceSigs #-}

newtype Moi s a = Moi
    { runMoi :: s -> (a, s)
    }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) =
        Moi $ \s ->
            let (state, init) = g s
            in (f state, init)
