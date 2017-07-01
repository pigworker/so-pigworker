No, that's not **mergeSort**. That's **insertionSort**, which is essentially the same algorithm as **bubbleSort**, depending on how you stare at it. At each step, a singleton list is `merge`d with the accumulated ordered-list-so-far, so, effectively, the element of that singleton is inserted.

As other commenters have already observed, to get **mergeSort** (and in particular, its efficiency), it's necessary to divide the problem repeatedly into roughly equal parts (rather than "one element" and "the rest"). The "official" solution gives a rather clunky way to do that. I quite like

    foldr (\ x (ys, zs) -> (x : zs, ys)) ([], [])

as a way to split a list in two, not in the middle, but into elements in even and odd positions.

If, like me, you like to have structure up front where you can see it, you can make ordered lists a `Monoid`.

    import Data.Monoid
    import Data.Foldable
    import Control.Newtype

    newtype Merge x = Merge {merged :: [x]}
    instance Newtype (Merge x) [x] where
      pack = Merge
      unpack = merged

    instance Ord x => Monoid (Merge x) where
      mempty = Merge []
      mappend (Merge xs) (Merge ys) = Merge (merge xs ys) where
        -- merge is as you defined it

And now you have insertion sort just by

    ala' Merge foldMap (:[]) :: [x] -> [x]

One way to get the divide-and-conquer structure of mergeSort is to make it a data structure: binary trees.

    data Tree x = None | One x | Node (Tree x) (Tree x) deriving Foldable

I haven't enforced a balancing invariant here, but I could. The point is that the same operation as before has another type

    ala' Merge foldMap (:[]) :: Tree x -> [x]

which merges lists collected from a treelike arrangement of elements. To obtain said arrangements, think "what's cons for `Tree`?" and make sure you keep your balance, by the same kind of twistiness I used in the above "dividing" operation.

    twistin :: x -> Tree x -> Tree x   -- a very cons-like type
    twistin x None        = One x
    twistin x (One y)     = Node (One x) (One y)
    twistin x (Node l r)  = Node (twistin x r) l

Now you have mergeSort by building a binary tree, then merging it.

    mergeSort :: Ord x => [x] -> [x]
    mergeSort = ala' Merge foldMap (:[]) . foldr twistin None

Of course, introducing the intermediate data structure has curiosity value, but you can easily cut it out and get something like

    mergeSort :: Ord x => [x] -> [x]
    mergeSort []   = []
    mergeSort [x]  = [x]
    mergeSort xs   = merge (mergeSort ys) (mergeSort zs) where
      (ys, zs) = foldr (\ x (ys, zs) -> (x : zs, ys)) ([], []) xs

where the tree has become the recursion structure of the program.
