import Control.Applicative (liftA2)

vignere :: String -> String -> String
vignere keyword "" = ""
vignere keyword str =
  unwords $ go (words str) keyword
    where
        go [] _ = []
        go (x:xs) keyword =
          take (length x) keyword : go xs (rotate (length x) keyword)
        rotate n =
          liftA2 (++) (drop n) (take n)
