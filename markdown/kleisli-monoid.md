In the business of library design, we face a choice point here, and we have chosen to be less than entirely consistent in our collective policy (or lack of it).

`Monoid` instances for `Monad` (or `Applicative`) type constructors can come about in a variety of ways. Pointwise lifting is always available, but we don't define

    instance (Applicative f, Monoid x) => Monoid (f x) {- not really -} where
      mempty         = pure mempty
      mappend fa fb  = mappend <$> fa <*> fb

Note that the `instance Monoid (a -> b)` is just such a pointwise lifting, so the pointwise lifting for `(a -> m b)` does happen whenever the monoid instance for `m b` does pointwise lifting for the monoid on `b`.

We don't do pointwise lifting in general, not only because it would prevent other `Monoid` instances whose carriers happen to be applied types, but also because the structure of the `f` is often considered more significant than that of the `x`. A key case in point is the *free* monoid, better known as `[x]`, which is a `Monoid` by `[]` and `(++)`, rather than by pointwise lifting. The monoidal structure comes from the list wrapping, not from the elements wrapped.

My preferred rule of thumb is indeed to prioritise monoidal structure inherent in the type constructor over either pointwise lifting, or monoidal structure of specific instantiations of a type, like the composition monoid for `a -> a`. These can and do get `newtype` wrappings.

Arguments break out over whether `Monoid (m x)` should coincide with `MonadPlus m` whenever both exist (and similarly with `Alternative`). My sense is that the only good `MonadPlus` instance is a copy of a `Monoid` instance, but others differ. Still, the library is not consistent in this matter, especially not in the matter of (many readers will have seen this old bugbear of mine coming)...

...the monoid instance for `Maybe`, which ignores the fact that we routinely use `Maybe` to model possible failure and instead observes that that the same data type idea of chucking in an extra element can be used to give a semigroup a neutral element if it didn't already have one. The two constructions give rise to isomorphic types, but they are not conceptually cognate. (**Edit** To make matters worse, the idea is implemented awkwardly, giving instance a `Monoid` constraint, when only a `Semigroup` is needed. I'd like to see the `Semigroup`-extends-to-`Monoid` idea implemented, but *not* for `Maybe`.)

Getting back to `Kleisli` in particular, we have three obvious candidate instances:

  1. `Monoid (Kleisli m a a)` with `return` and Kleisli composition
  2. `MonadPlus m => Monoid (Kleisli m a b)` lifting `mzero` and `mplus` pointwise over `->`
  3. `Monoid b => Monoid (Kleisli m a b)` lifting the monoid structure of `b` over `m` then `->`

I expect no choice has been made, just because it's not clear which choice to make. I hesitate to say so, but my vote would be for 2, prioritising the structure coming from `Kleisli m a` over the structure coming from `b`.
