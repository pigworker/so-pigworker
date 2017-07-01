In recent Haskell, I'd switch on a bit of kitchen sink.

    {-# LANGUAGE PolyKinds, DataKinds, GADTs, KindSignatures,
        TypeOperators, PatternSynonyms #-}

Then I'd define type-level list membership

    data (:>) :: [x] -> x -> * where
      Ze ::            (x ': xs) :> x
      Su :: xs :> x -> (y ': xs) :> x

and now I have all the finite sums, without cranking out a whole raft of OneOfN type definitions:

    data Sum :: [*] -> * where
      (:-) :: xs :> x -> x -> Sum xs

But, to address Tomas's issue about readability, I'd make use of pattern synonyms. Indeed, this sort of thing is the reason I've been banging on about pattern synonyms for years.

You can have a funny version of `Maybe`:

    type MAYBE x = Sum '[(), x]

    pattern NOTHING :: MAYBE x
    pattern NOTHING = Ze :- ()

    pattern JUST :: x -> MAYBE x
    pattern JUST x = Su Ze :- x

and you can even use `newtype` to build recursive sums.

    newtype Tm x = Tm (Sum '[x, (Tm x, Tm x), Tm (Maybe x)])

    pattern VAR :: x -> Tm x
    pattern VAR x = Tm (Ze :- x)

    pattern APP :: Tm x -> Tm x -> Tm x
    pattern APP f s = Tm (Su Ze :- (f, s))

    pattern LAM :: Tm (Maybe x) -> Tm x
    pattern LAM b = Tm (Su (Su Ze) :- b)

The newtype wrapper also lets you make `instance` declaration for types built that way.

You can, of course, also use pattern synonyms to hide an iterated `Either` nicely.

This technique is not exclusive to sums: you can do it for products, too, and that's pretty much what happens in de Vries and Löh's [Generics-SOP][1] library.

The big win from such an encoding is that the description of data is itself (type-level) data, allowing you to cook up lots of `deriving`-style functionality without hacking the compiler.

In the future (if I have my way), all datatypes will be *defined*, not *declared*, with datatype descriptions made of data specifiying both the algebraic structure (allowing generic equipment to be computed) of the data and its appearance (so you can see what you're doing when working with a specific type).

But the future is sort of here already.

  [1]: https://hackage.haskell.org/package/generics-sop
