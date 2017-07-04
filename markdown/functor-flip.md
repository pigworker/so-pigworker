The future is now, when you (use ghc 8 and) switch on a flag or two

    Prelude> :set -XPolyKinds -XFlexibleInstances

Let us declare

    Prelude> newtype Flip f a b = MkFlip (f b a)

and then enquire

    Prelude> :kind Flip
    Flip :: (k1 -> k -> *) -> k -> k1 -> *

    Prelude> :type MkFlip
    MkFlip
      :: forall k k1 (b :: k) (f :: k -> k1 -> *) (a :: k1).
         f b a -> Flip f a b

The *type* constructor `Flip` takes two implicit arguments, being `k` and `k1`, and three explicit arguments, being a binary function producing a type, then its two arguments in reverse order. The arguments to this function are of unconstrained type (old people can say "kind" if they like), but it certainly returns a type (in the strict sense of "thing in `*`", rather than the uselessly vague sense of "any old rubbish right of `::`") because it is certainly used as a type in the declaration of `MkFlip`.

The *data* constructor, `MkFlip`, takes *five* implicit arguments (exactly the arguments of `Flip`) and one explicit argument, being some data in `f b a`.

What's going on is Hindley-Milner type inference one level up. Constraints are collected (e.g., `f b a` must inhabit `*` because a constructor argument must inhabit `f b a`) but otherwise a most general type is delivered: `a` and `b` could be anything, so their types are generalised as `k1` and `k`.

Let's play the same game with the constant type constructor:

    Prelude> newtype K a b = MkK a

    Prelude> :kind K
    K :: * -> k -> *

    Prelude> :type MkK
    MkK :: forall k (b :: k) a. a -> K a b

We see that `a :: *` but `b` can be any old rubbish (and for that matter, `k :: *`, as these days, `* :: *`). Clearly, `a` is actually used as the type of a thing, but `b` is not used at all, hence unconstrained.

We may then declare

    Prelude> instance Functor (Flip K b) where fmap f (MkFlip (MkK a)) = MkFlip (MkK (f a))

and ask

    Prelude> :info Flip
    ...
    instance [safe] forall k (b :: k). Functor (Flip K b)

which tells us that the unused `b` can still be any old rubbish. Because we had

    K    ::   * -> k -> *
    Flip :: (k1 -> k -> *) -> k -> k1 -> *

we can unify `k1 = *` and get

    Flip K :: k -> * -> *

and so

    Flip K b :: * -> *

for any old `b`. A `Functor` instance is thus plausible, and indeed deliverable, with the function acting on the packed up `a` element, corresponding to the argument of `Flip K b` which becomes the *first* argument of `K`, hence the type of the stored element.

Unification-based type inference is alive and (fairly) well, right of `::`.
