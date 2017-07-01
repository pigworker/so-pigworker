If we compare the types

    (<*>) :: Applicative a => a (s -> t) -> a s -> a t
    (>>=) :: Monad m =>       m s -> (s -> m t) -> m t

we get a clue to what separates the two concepts. That `(s -> m t)` in the type of `(>>=)` shows that a value in `s` can determine the behaviour of a computation in `m t`. Monads allow interference between the value and computation layers. The `(<*>)` operator allows no such interference: the function and argument computations don't depend on values. This really bites. Compare

    miffy :: Monad m => m Bool -> m x -> m x -> m x
    miffy mb mt mf = do
      b <- mb
      if b then mt else mf

which uses the result of some effect to decide between two *computations* (e.g. launching missiles and signing an armistice), whereas

    iffy :: Applicative a => a Bool -> a x -> a x -> a x
    iffy ab at af = pure cond <*> ab <*> at <*> af where
      cond b t f = if b then t else f

which uses the value of `ab` to choose between *the values of* two computations `at` and `af`, having carried out both, perhaps to tragic effect.

The monadic version relies essentially on the extra power of `(>>=)` to choose a computation from a value, and that can be important. However, supporting that power makes monads hard to compose. If we try to build &lsquo;double-bind&rsquo;

    (>>>>==) :: (Monad m, Monad n) => m (n s) -> (s -> m (n t)) -> m (n t)
    mns >>>>== f = mns >>-{-m-} \ ns -> let nmnt = ns >>= (return . f) in ???

we get this far, but now our layers are all jumbled up. We have an `n (m (n t))`, so we need to get rid of the outer `n`. As Alexandre C says, we can do that if we have a suitable

    swap :: n (m t) -> m (n t)

to permute the `n` inwards and `join` it to the other `n`.

The weaker &lsquo;double-apply&rsquo; is much easier to define

    (<<**>>) :: (Applicative a, Applicative b) => a (b (s -> t)) -> a (b s) -> a (b t)
    abf <<**>> abs = pure (<*>) <*> abf <*> abs

because there is no interference between the layers.

Correspondingly, it's good to recognize when you really need the extra power of `Monad`s, and when you can get away with the rigid computation structure that `Applicative` supports.

Note, by the way, that although composing monads is difficult, it might be more than you need. The type `m (n v)` indicates computing with `m`-effects, then computing with `n`-effects to a `v`-value, where the `m`-effects finish before the `n`-effects start (hence the need for `swap`). If you just want to interleave `m`-effects with `n`-effects, then composition is perhaps too much to ask!
