I would discourage talk of "the Hask Category" because it subconsciously conditions you against looking for other categorical structure in Haskell programming.

Indeed, rose trees can be seen as the fixpoint of an endofunctor on types-and-functions, a category which we might be better to call `Type`, now that `Type` is the type of types. If we give ourselves some of the usual functor kit...

    newtype K a   x = K a deriving Functor           -- constant functor
    newtype P f g x = P (f x, g x) deriving Functor  -- products

...and fixpoints...

    newtype FixF f = InF (f (FixF f))

...then we may take

    type Rose a = FixF (P (K a) [])
    pattern Node :: a -> [Rose a] -> Rose a
    pattern Node a ars = InF (P (K a, ars))

The fact that `[]` is itself recursive does not prevent its use in the formation of recursive datatypes via `Fix`. To spell out the recursion explicitly, we have nested fixpoints, here with bound variable names chosen suggestively:

    Rose a = μrose. a * (μlist. 1 + (rose * list))

Now, by the time we've arrived in the second fixpoint, we have a type formula

    1 + (rose * list)

which is functorial (indeed, strictly positive) in both `rose` and `list`. One might say it is a `Bifunctor`, but that's unnecessary terminology: it's a functor from `(Type, Type)` to `Type`. You can make a `Type -> Type` functor by taking a fixpoint in the second component of the pair, and that's just what happened above. 

The above definition of `Rose` loses an important property. It is not true that

    Rose :: Type -> Type   -- GHC might say this, but it's lying

merely that `Rose x :: Type` if `x :: Type`. In particular,

    Functor Rose

is not a well typed constraint, which is a pity, as intuitively, rose trees ought to be functorial in the elements they store.

You can fix this by building `Rose` as itself being the fixpoint of a `Bifunctor`. So, in effect, by the time we get to lists, we have *three* type variables in scope, `a`, `rose` and `list`, and we have functoriality in all of them. You need a *different* fixpoint type constructor, and a *different* kit for building `Bifunctor` instances: for `Rose`, life gets easier because the `a` parameter is not used in the inner fixpoint, but in general, to define bifunctors as fixpoints requires trifunctors, and off we go!

[This answer](https://stackoverflow.com/a/45257691/828361) of mine shows how to fight the proliferation by showing how *indexed* types are *closed* under a fixpoint-of-functor construction. That's to say, work not in `Type` but in `i -> Type` (for the full variety of index types `i`) and you're ready for mutual recursion, GADTs, and so on.

So, zooming out, rose trees are given by mutual fixpoints, which have a perfectly sensible categorical account, provided you see which categories are actually at work.
