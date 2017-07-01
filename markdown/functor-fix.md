This is an old sore for me. The crucial point is that your `ExprF` is functorial in *both* its parameters. So if we had

    class Bifunctor b where
      bimap :: (x1 -> y1) -> (x2 -> y2) -> b x1 x2 -> b y1 y2

then you could define (or imagine a machine defining for you)

    instance Bifunctor ExprF where
      bimap k1 k2 (Val a)    = Val (k1 a)
      bimap k1 k2 (Add x y)  = Add (k2 x) (k2 y)

and now you can have

    newtype Fix2 b a = MkFix2 (b a (Fix2 b a))

accompanied by

    map1cata2 :: Bifunctor b => (a -> a') -> (b a' t -> t) -> Fix2 b a -> t
    map1cata2 e f (MkFix2 bar) = f (bimap e (map1cata2 e f) bar)

which in turn gives you that when you take a fixpoint in one of the parameters, what's left is still functorial in the other

    instance Bifunctor b => Functor (Fix2 b) where
      fmap k = map1cata2 k MkFix2

and you sort of get what you wanted. But your `Bifunctor` instance isn't going to be built by magic. And it's a bit annoying that you need a different fixpoint operator and a whole new kind of functor. The trouble is that you now have *two* sorts of substructure: "values" and "subexpressions".

**And here's the turn.** There is a notion of functor which is *closed* under fixpoints. Turn on the kitchen sink (especially `DataKinds`) and

    type s :-> t = forall x. s x -> t x

    class FunctorIx (f :: (i -> *) -> (o -> *)) where
      mapIx :: (s :-> t) -> f s :-> f t

Note that "elements" come in a kind indexed over `i` and "structures" in a kind indexed over some other `o`. We take `i`-preserving functions on elements to `o` preserving functions on structures. Crucially, `i` and `o` can be different.

The magic words are "1, 2, 4, 8, time to exponentiate!". A type of kind `*` can easily be turned into a trivially indexed GADT of kind `() -> *`. And two types can be rolled together to make a GADT of kind `Either () () -> *`. That means we can roll both sorts of substructure together. In general, we have a kind of type level `either`.

    data Case :: (a -> *) -> (b -> *) -> Either a b -> * where
      CL :: f a -> Case f g (Left a)
      CR :: g b -> Case f g (Right b)

equipped with its notion of "map"

    mapCase :: (f :-> f') -> (g :-> g') -> Case f g :-> Case f' g'
    mapCase ff gg (CL fx) = CL (ff fx)
    mapCase ff gg (CR gx) = CR (gg gx)

So we can refunctor our bifactors as `Either`-indexed `FunctorIx` instances.

And now we can take the fixpoint of any node structure `f` which has places for either elements `p` or subnodes. It's just the same deal we had above.

    newtype FixIx (f :: (Either i o -> *) -> (o -> *))
                  (p :: i -> *)
                  (b :: o)
      = MkFixIx (f (Case p (FixIx f p)) b)

    mapCata :: forall f p q t. FunctorIx f =>
      (p :-> q) -> (f (Case q t) :-> t) -> FixIx f p :-> t
    mapCata e f (MkFixIx node) = f (mapIx (mapCase e (mapCata e f)) node)

But now, we get the fact that `FunctorIx` is closed under `FixIx`.

    instance FunctorIx f => FunctorIx (FixIx f) where
      mapIx f = mapCata f MkFixIx

Functors on indexed sets (with the extra freedom to vary the index) can be very precise and very powerful. They enjoy many more convenient closure properties than `Functor`s do. I don't suppose they'll catch on.
