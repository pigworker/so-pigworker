Yes, that's `para`. Compare with catamorphism, or `foldr`:

    para  :: (a -> [a] -> b -> b) -> b -> [a] -> b
    foldr :: (a ->        b -> b) -> b -> [a] -> b

    para  c n (x : xs) = c x xs (para c n xs)
    foldr c n (x : xs) = c x    (foldr c n xs)
    para  c n []       = n
    foldr c n []       = n

Some people call paramorphisms "primitive recursion" by contrast with catamorphisms (`foldr`) being "iteration".

Where `foldr`'s two parameters are given a recursively computed value for each recursive subobject of the input data (here, that's the tail of the list), `para`'s parameters get both the original subobject and the value computed recursively from it.

An example function that's nicely expressed with `para` is the collection of the proper suffices of a list.

    suff :: [x] -> [[x]]
    suff = para (\ x xs suffxs -> xs : suffxs) []

so that

    suff "suffix" = ["uffix", "ffix", "fix", "ix", "x", ""]

Possibly simpler still is

    safeTail :: [x] -> Maybe [x]
    safeTail = para (\ _ xs _ -> Just xs) Nothing

in which the "cons" branch ignores its recursively computed argument and just gives back the tail. Evaluated lazily, the recursive computation never happens and the tail is extracted in constant time.

You can define `foldr` using `para` quite easily; it's a little trickier to define `para` from `foldr`, but it's certainly possible, and everyone should know how it's done!

    foldr c n =       para  (\ x  xs  t ->           c x    t)       n
    para  c n = snd . foldr (\ x (xs, t) -> (x : xs, c x xs t)) ([], n)

The trick to defining `para` with `foldr` is to reconstruct a *copy* of the original data, so that we gain access to a copy of the tail at each step, even though we had no access to the original. At the end, `snd` discards the copy of the input and gives just the output value. It's not very efficient, but if you're interested in sheer expressivity, `para` gives you no more than `foldr`. If you use this `foldr`-encoded version of `para`, then `safeTail` will take linear time after all, copying the tail element by element.

So, that's it: `para` is a more convenient version of `foldr` which gives you immediate access to the tail of the list as well as the value computed from it.

In the general case, working with a datatype generated as the recursive fixpoint of a functor

    data Fix f = In (f (Fix f))

you have

    cata :: Functor f => (f         t  -> t) -> Fix f -> t
    para :: Functor f => (f (Fix f, t) -> t) -> Fix f -> t

    cata phi (In ff) = phi (fmap (cata phi) ff)
    para psi (In ff) = psi (fmap keepCopy   ff) where
      keepCopy x = (x, para psi x)

and again, the two are mutually definable, with `para` defined from `cata` by the same "make a copy" trick

    para psi = snd . cata (\ fxt -> (In (fmap fst fxt), psi fxt))

Again, `para` is no more expressive than `cata`, but more convenient if you need easy access to substructures of the input.

**Edit:** I remembered another nice example.

Consider binary search trees given by `Fix TreeF` where

    data TreeF sub = Leaf | Node sub Integer sub

and try defining insertion for binary search trees, first as a `cata`, then as a `para`. You'll find the `para` version much easier, as at each node you will need to insert in one subtree but preserve the other as it was.
