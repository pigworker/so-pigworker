A crucial skill in functional programming is grasping, in a given situation, the right way to do nothing. Classic errors arise, e.g., from giving `[]` meaning "no solutions" as the base case of a recursive search, when `[[]]` meaning "one trivial solution" is needed. Thus also `Functor` instances for phantom types, i.e. constant functors, are useful as the apparently trivial base case of a larger pattern.

I can present the general toolkit for building container-like structures as follows:

    newtype K a x = K a deriving Functor             -- K for konstant
    {- fmap _ (K a) = K a -}

    newtype I x = I x deriving Functor               -- I for identity
    {- fmap k (I x) = I (k x) -}

    newtype P f g x = P (f x, g x) deriving Functor  -- P for product
    {- will give (Functor f, Functor g) => Functor (P f g), such that
       fmap k (P (fx, gx)) = P (fmap k fx, fmap k gx) -}

    newtype S f g x = S (Either (f x) (g x))         -- S for sum
    instance (Functor f, Functor g) => Functor (S f g) where
      fmap k (S (Left  fx))  = S (Left  (fmap k fx))
      fmap k (S (Right gx))  = S (Right (fmap k gx))

Now, any recursive data structure can be presented as a top node which acts as a container for substructures.

    data Data f = Node (f (Data f))

For example, if I want to make binary trees with numbers at the leaves, I can
write

    type Tree = S (K Int) (P I I)

to indicate that the node structure for a tree is either a leaf with an `Int` and *no subtrees* or a fork with a pair of subtrees. I need `K` to point out the *absence* of recursive substructures. The type of trees is then `Data Tree`.

The usual recursion scheme for these things is

    fold :: Functor f => (f t -> t) -> Data f -> t
    fold k (Node fd) = k (fmap (fold k) fd)

We don't need to do any work to instantiate that for trees, because `Tree` is already a `Functor`, as it was built from functorial components. The trivial `fmap` for `K Int` amounts to saying that the recursion stops when you reach a leaf.

Of course, these "encoded" data types make it harder to see what you're doing when you write ordinary programs by pattern matching. That's where the `PatternSynonyms` extension comes to your rescue. You can say

    pattern Leaf i    = Node (S (Left (K i)))
    pattern Fork l r  = Node (S (Right (P (I l, I r))))

to recover the usual interface. I recommend leaving off the outer `Node`, to fit with the way `fold` strips `Node` for you.

    pattern Leaf i = S (Left (K i))
    pattern Fork l r = S (Right (P (I l, I r)))

    add :: Data Tree -> Int
    add = fold $ \ t -> case t of
      Leaf i   -> i
      Fork x y -> x + y

I've barely scratched the surface of the kinds of generic functionality you can roll out to lots of data types whenever you can develop them just for `K`, `I`, `P` and `S`. The `K` cases are always trivial, but they have to be there.

Similar considerations apply to the `Void` data type (in `Data.Void`). Why on earth would we bother to introduce a data type with no elements worth speaking of? To model the impossible cases of a larger scheme.
