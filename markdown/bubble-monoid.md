I'm using my phone with a poor network connection, but here goes.

**tl;dr** bubblesort is insertion sort is the monoidal "crush" for the monoid of ordered lists with merging.

Ordered lists form a monoid.

    newtype OL x = OL [x]
    instance Ord x => Monoid (OL x) where
      mempty = OL []
      mappend (OL xs) (OL ys) = OL (merge xs ys) where
        merge [] ys = ys
        merge xs [] = xs
        merge xs@(x : xs') ys@(y : ys')
           | x <= y = x : merge xs' ys
           | otherwise = y : merge xs ys'

Insertion sort is given by

    isort :: Ord x => [x] -> OL x
    isort = foldMap (OL . pure)

because insertion is exactly merging a singleton list with another list. (Mergesort is given by building a balanced tree, then doing the same foldMap.)

What has this to do with bubblesort? Insertion sort and bubblesort have exactly the same comparison strategy. You can see this if you draw it as a sorting network made from compare-and-swap boxes. Here, data flows downward and lower inputs to boxes [n] go left:

    | | | |
    [1] | |
    | [2] |
    [3] [4]
    | [5] |
    [6] | |
    | | | |

If you perform the comparisons in the sequence given by the above numbering, cutting the diagram in / slices, you get insertion sort: the first insertion needs no comparison; the second needs comparison 1; the third 2,3; the last 4,5,6. 

But if, instead, you cut in \ slices...

    | | | |
    [1] | |
    | [2] |
    [4] [3]
    | [5] |
    [6] | |
    | | | |

...you are doing bubblesort: first pass 1,2,3; second pass 4,5; last pass 6.
