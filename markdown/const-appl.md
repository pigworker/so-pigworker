It's rather useful when combined with `Traversable`.

    getConst . traverse Const :: (Monoid a, Traversable f) => f a -> a

That's the general recipe for glomming a bunch of stuff together. It was one of the use cases which convinced me that it was worth separating `Applicative` from `Monad`. I needed stuff like generalized `elem`

    elem :: Eq x => x -> Term x -> Bool

to do occur-checking for a `Traversable Term` parametrized by the representation of free variables. I kept changing the representation of `Term` and I was fed up modifying a zillion traversal functions, some of which were doing accumulations, rather than effectful mapping. I was glad to find an abstraction which covered both.
