data Tree elem = Empty
  | Node (Tree elem) elem (Tree elem)

Eq elem => Eq (Tree elem) where
  Empty == Empty = True
  (Node left e right) == (Node left' e' right') = left == left' && e == e' && right == right'
  _ == _ = False

Functor Tree where
  map f Empty = Empty
  map f (Node left e right) = Node (map f left) (f e) (map f right)

Foldable Tree where
  foldr f acc Empty = acc
  foldr f acc (Node left e right) =
    -- calculate sum for leafs (rightFold takes initial val from result of leftFold)
    let leftFold = foldr f acc left
        rightFold = foldr f leftFold right in
    f e rightFold

testTree: Tree Nat
testTree = Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)
-- foldr (+) 0 testTree
-- outputs 6: Nat
