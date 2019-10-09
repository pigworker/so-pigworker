Here's a way to do it without flattening the tree.

From the definition, here,

    data BinaryTree a = Null | Node (BinaryTree a) a (BinaryTree a)
         deriving Show

one can see that traversing the tree left to right, ignoring `Node` and parentheses, gives you an alternating sequence of `Null`s and `a`s. That is, between every two values, there is a `Null`.

My plan is to check that each subtree satisfies suitable *requirements*: we can *refine* the requirements at each `Node`, remembering which values we are between, then *test* them at each `Null`. As there is a `Null` between every in order pair of values, we will have tested that all in order (left-to-right) pairs are non-decreasing.

What is a requirement? It's a *loose* lower and upper bound on the values in the tree. To express requirements, including those at the leftmost and rightmost ends, we may extend any ordering with `Bot`tom and `Top` elements, as follows:

    data TopBot a = Bot | Val a | Top deriving (Show, Eq, Ord)

Now let us check that a given tree satisfies the requirements of being both in order and between given bounds.

    ordBetween :: Ord a => TopBot a -> TopBot a -> BinaryTree a -> Bool
      -- tighten the demanded bounds, left and right of any Node
    ordBetween lo hi (Node l x r) = ordBetween lo (Val x) l && ordBetween (Val x) hi l
      -- check that the demanded bounds are in order when we reach Null
    ordBetween lo hi Null         = lo <= hi

A binary search tree is a tree that is in order and between `Bot` and `Top`.

    isBSTree :: Ord a => BinaryTree a -> Bool
    isBSTree = ordBetween Bot Top

Computing the *actual* extremal values in each subtree, bubbling them outwards, gives you more information than you need, and is fiddly in the edge cases where a left or right subtree is empty. Maintaining and checking the *requirements*, pushing them inwards, is rather more uniform.
