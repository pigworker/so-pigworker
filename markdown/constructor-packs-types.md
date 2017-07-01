At the moment, I'm afraid not. I haven't spotted an obvious workaround, either.

[This ticket](https://ghc.haskell.org/trac/ghc/ticket/6024) documents the prospects for the declaration of data kinds, born kind, rather than being data types with kindness thrust upon them. It would be entirely reasonable for the constructors of such things to pack up types as you propose. We're not there yet, but it doesn't look all that problematic.

My eyes are on a greater prize. I would like * to be perfectly sensible type of runtime values, so that the kind you want could exist by promotion as we have it today. Combine that with the mooted notion of `pi`-type (non-parametric abstraction over the portion of the language that's effectively shared by types and values) and we might get a more direct way to make ad hoc type abstractions than we have with `Data.Typeable`. The usual `forall` would remain parametric.
