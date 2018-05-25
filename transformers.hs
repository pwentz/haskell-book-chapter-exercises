import Control.Monad.Trans.Reader

rDec :: Num a => Reader a a
rDec = return $ \r -> r - 1
