data BinaryTree a
    = Leaf
    | Node (BinaryTree a)
           a
           (BinaryTree a)
    deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f Leaf = Leaf
mapTree f (Node leftTree root rightTree) =
    Node (mapTree f leftTree) (f root) (mapTree f rightTree)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
        then putStrLn "Preorder fine!"
        else putStrLn "PREORDER FAILS!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node ltree root rtree) = root : (preorder ltree) ++ (preorder rtree)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ init Leaf = init
foldTree f init (Node ltree root rtree) =
    foldTree f (f root (foldTree f init rtree)) ltree

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold fn val = maybe Leaf recurse (fn val)
  where
    recurse (left, root, right) = Node (unfold fn left) root (unfold fn right)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold go n
  where
    go n'
        | n' < 1 = Nothing
        | otherwise = Just (n' - 1, n - n', n' - 1)
