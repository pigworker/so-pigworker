If the `Traversable` and `Foldable` instances for `(,) x)` were in the library (and I suppose I must take some blame for their absence)...

    instance Traversable ((,) x) where
      traverse f (x, y) = (,) x <$> f y

    instance Foldable ((,) x) where
      foldMap = foldMapDefault

...then this (sometimes called 'strength') would be a specialisation of `Data.Traversable.sequence`.

    sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
so

    sequence :: (Monad m) => ((,) x) (m a) -> m (((,) x) a)
i.e.

    sequence :: (Monad m) => (x, m a) -> m (x, a)

In fact, sequence doesn't really use the full power of `Monad`: `Applicative` will do. Moreover, in this case, pairing-with-x is linear, so the `traverse` does only `<$>` rather than other random combinations of `pure` and `<*>`, and (as has been pointed out elsewhere) you only need `m` to have functorial structure.
