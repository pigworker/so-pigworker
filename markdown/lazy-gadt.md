Consider

    data EQ a b where
      Refl :: EQ a a

If we could define

    transport :: Eq a b -> a -> b
    transport ~Refl a = a

then we could have

    transport undefined :: a -> b

and thus acquire

    transport undefined True = True :: Int -> Int

and then a crash, rather than the more graceful failure you get when trying to head-normalise the `undefined`.

GADT data represent evidence *about* types, so bogus GADT data threaten type safety. It is necessary to be strict with them to validate that evidence: you can't trust unevaluated computations in the presence of bottom.
