I don't know the details of Morte, but I have some clues about what's possible in typed lambda-calculi more broadly.

If `Nat` is defined impredicatively, it might be possible to define these trees by iteration.

    Nat : *
    Nat = (x : *) -> (x -> x) -> x -> x
    Pair : * -> * -> *
    Pair x y = (z : *) -> (x -> y -> z) -> z
    Tree : * -> Nat -> *
    Tree a n = n * (\ t -> Pair t t) a

Of course, to get away with that, I need a *large* elimination. Here, I've casually just taken `* : *`, but that's not safe in general. Inductive definitions admit large eliminations unproblematically: impredicatively encoded datatypes, not so.

But, above, I exploit the fact that the indexing structure of the trees happens to be compatible with that of the `Nat`s which index them, and there is no reason why that should be the case in general. Indices vary in all sorts of wacky ways: it's only those that characterize some sort of "size" that get smaller as we go inward.

Indexed structures do admit a Church encoded presentation. It's just that instead of iterating over a set, we iterate over an indexed set. Here's one way to express it.

    Tree : * -> Nat -> *
    Tree a n = (x : Nat -> *) ->
               (a -> x Z) ->
               ((n : Nat) -> x n -> x n -> x (S n)) ->
               x n

It's easy to write some things like

    leftmost : (a : *) -> (n : Nat) -> Tree a n -> a
    leftmost a n t = t (\ _ -> a) (\ a -> a) (\ _ l _ -> l)

but

    leftChild : (a : *) -> (n : Nat) -> Tree a (S n) -> Tree a n

is a taller order, requiring some way to inspect or constrain numbers. That's why GHC Haskell has all this stuff about equality, `~`.
