import Control.Applicative (liftA2)
import qualified Data.Char as Char
import Data.Maybe (fromMaybe)

-- Chapter 9 - Lists
--  - list constructor (& syntactic sugar)
--  - destructuring/pattern matching
--  - ranges/enums
--  - extracting from list (take, drop, splitAt)
--  - list comprehension
--  - spines & non-strictness evaluation
--  - normal form vs whnf
--  - transforming lists of values (map)
--  - filtering lists of value
--  - zip list
-- pg 308
--
eft :: (Enum a, Eq a, Ord a) => a -> a -> [a]
eft from to
  | from > to = []
  | from == to = [to]
  | otherwise = from : eft (succ from) to
