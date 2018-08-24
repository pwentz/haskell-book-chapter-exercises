data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) =
  (a : preorder left) ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) =
  inorder left ++ (a : inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) =
  postorder left ++ postorder right ++ [a]

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
