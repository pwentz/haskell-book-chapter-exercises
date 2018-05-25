import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Optional a
    = None
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = None
    mappend x y =
        case (x, y) of
            (Only x', Only y') -> Only $ mappend x' y'
            (None, y') -> y'
            (x', None) -> x'

type Verb = String

type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
    mconcat
        [ e
        , "! he said"
        , adv
        , " as he jumped into his car "
        , noun
        , " and drove off with his "
        , adj
        , " wife."
        ]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- Bool Fail check
data Bull
    = Fools
    | Twoo
    deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
    let ma = monoidAssoc
        mli = monoidLeftIdentity
        mlr = monoidRightIdentity
    quickCheck (ma :: BullMappend)
    quickCheck (mli :: Bull -> Bool)
    quickCheck (mlr :: Bull -> Bool)
