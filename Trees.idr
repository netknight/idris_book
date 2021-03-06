||| A binary tree
data Tree elem = Empty
  | Node (Tree elem) elem (Tree elem)

%name Tree left, right

insert: Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                    LT => Node (insert x left) val right
                                    EQ => orig --Node left val right
                                    GT => Node left val (insert x right)

listToTree: Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList: Ord a => Tree a -> List a
treeToList Empty = []
treeToList (Node left x right) = treeToList left ++ (x :: treeToList right)
