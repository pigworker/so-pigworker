I wrote a talk on this topic called ["Slicing It"](http://strictlypositive.org/slicing-jpgs/) in 2009. It certainly points to the work by my Strathclyde colleagues, Johann and Ghani, on initial algebra semantics for GADTs. I used the notation which [SHE](https://personal.cis.strath.ac.uk/conor.mcbride/pub/she/) provides for writing data-indexed types, but that has pleasingly been superseded by the "promotion" story.

The key point of the talk is, as per my comment, to be systematic about using exactly one index, but to exploit the fact that its kind can vary.

So indeed, we have (using my current preferred "Goscinny and Uderzo" names)

    type s :-> t = forall i. s i -> t i

    class FunctorIx f where
      mapIx :: (s :-> t) -> (f s :-> f t)

Now you can show `FunctorIx` is *closed* under fixpoints. The key is to combine two indexed sets into a one that offers a choice of index.

    data Case (f :: i -> *) (g :: j -> *) (b :: Either i j) :: * where
      L :: f i -> Case f g (Left i)
      R :: g j -> Case f g (Right j)

    (<?>) :: (f :-> f') -> (g :-> g') -> Case f g :-> Case f' g'
    (f <?> g) (L x) = L (f x)
    (f <?> g) (R x) = R (g x)

Now we can now take fixpoints of functors whose "contained elements" stand for either "payload" or "recursive substructures".

    data MuIx (f :: (Either i j -> *) -> j -> *) :: (i -> *) -> j -> * where
      InIx :: f (Case x (MuIx f x)) j -> MuIx f x j

As a result, we can `mapIx` over "payload"...

    instance FunctorIx f => FunctorIx (MuIx f) where
      mapIx f (InIx xs) = InIx (mapIx (f <?> mapIx f) xs)

...or write a catamorphism over the "recursive substructures"...

    foldIx :: FunctorIx f => (f (Case x t) :-> t) -> MuIx f x :-> t
    foldIx f (InIx xs) = f (mapIx (id <?> foldIx f) xs)

...or both at once.

    mapFoldIx :: FunctorIx f => (x :-> y) -> (f (Case y t) :-> t) -> MuIx f x :-> t
    mapFoldIx e f (InIx xs) = f (mapIx (e <?> mapFoldIx e f) xs)

The joy of `FunctorIx` is that it has such splendid closure properties, thanks to the ability to vary the indexing kinds. `MuIx` allows for notions of payload, and can be iterated. There is thus an incentive to work with structured indices rather than multiple indices.
