Your first attempt is *not* using existential types. Rather your

    lists :: [(Int, forall a. Show a => Int -> a)]

demands that the second components can deliver an element of *any* showable type that I choose, not just *some* showable type that you choose. You're looking for

    lists :: [(Int, exists a. Show a * (Int -> a))]  -- not real Haskell

but that's not what you've said. The datatype packaging method allows you to recover `exists` from `forall` by currying. You have

    HRF :: forall a. Show a => (Int -> a) -> HRF

which means that to build an `HRF` value, you must supply a triple containing a type `a`, a `Show` dictionary for `a` and a function in `Int -> a`. That is, the `HRF` constructor's type effectively curries this non-type

    HRF :: (exists a. Show a * (Int -> a)) -> HRF   -- not real Haskell

You might be able to avoid the datatype method by using rank-n types to Church-encode the existential

    type HRF = forall x. (forall a. Show a => (Int -> a) -> x) -> x

but that's probably overkill.
