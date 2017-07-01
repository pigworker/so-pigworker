I see we're accumulating over some data structure. I think `foldMap`. I ask "Which `Monoid`"? It's some kind of lists of accumulations. Like this

    newtype Bunch x = Bunch {bunch :: [x]}
    instance Semigroup x => Monoid (Bunch x) where
      mempty = Bunch []
      mappend (Bunch xss) (Bunch yss) = Bunch (glom xss yss) where
        glom [] yss = yss
        glom xss [] = xss
        glom (xs : xss) (ys : yss) = (xs <> ys) : glom xss yss

Our underlying elements have some associative operator `<>`, and we can thus apply that operator pointwise to a pair of lists, just like `zipWith` does, except that when we run out of one of the lists, *we don't truncate*, rather we just take the other. Note that `Bunch` is a name I'm introducing for purposes of this answer, but it's not that unusual a thing to want. I'm sure I've used it before and will again.

If we can translate

    0 -> Bunch [[0]]           -- single 0 in place 0
    1 -> Bunch [[],[1]]        -- single 1 in place 1
    2 -> Bunch [[],[],[2]]     -- single 2 in place 2
    3 -> Bunch [[],[],[],[3]]  -- single 3 in place 3
    ...

and `foldMap` across the input, then we'll get the right number of each in each place. There should be no need for an upper bound on the numbers in the input to get a sensible output, as long as you are willing to interpret `[]` as "the rest is silence". Otherwise, like Procrustes, you can pad or chop to the length you need.

Note, by the way, that when `mappend`'s first argument comes from our translation, we do a bunch of `([]++)` operations, a.k.a. `id`s, then a single `([i]++)`, a.k.a. `(i:)`, so if `foldMap` is right-nested (which it is for lists), then we will always be doing cheap operations at the left end of our lists.

Now, as the question works with lists, we might want to introduce the `Bunch` structure only when it's useful. That's what `Control.Newtype` is for. We just need to tell it about `Bunch`.

    instance Newtype (Bunch x) [x] where
      pack = Bunch
      unpack = bunch

And then it's

    groupInts :: [Int] -> [[Int]]
    groupInts = ala' Bunch foldMap (basis !!) where
      basis = ala' Bunch foldMap id [iterate ([]:) [], [[[i]] | i <- [0..]]]

What? Well, without going to town on what `ala'` is in general, its impact here is as follows:

    ala' Bunch foldMap f = bunch . foldMap (Bunch . f)

meaning that, although `f` is a function to lists, we accumulate as if `f` were a function to `Bunch`es: the role of `ala'` is to insert the correct `pack` and `unpack` operations to make that just happen.

We need `(basis !!) :: Int -> [[Int]]` to be our translation. Hence `basis :: [[[Int]]]` is the list of images of our translation, computed on demand at most once each (i.e., the translation, *memoized*).

For this `basis`, observe that we need these two infinite lists

    [ []                    [ [[0]]
    , [[]]                  , [[1]]
    , [[],[]]               , [[2]]
    , [[],[],[]]            , [[3]]
    ...                     ...

combined `Bunch`wise. As both lists have the same length (infinity), I could also have written

    basis = zipWith (++) (iterate ([]:) []) [[[i]] | i <- [0..]]

but I thought it was worth observing that this also is an example of `Bunch` structure.

Of course, it's very nice when something like `accumArray` hands you exactly the sort of accumulation you need, neatly packaging a bunch of grungy behind-the-scenes mutation. But the general recipe for an accumulation is to think "What's the `Monoid`?" and "What do I do with each element?". That's what `foldMap` asks you.
