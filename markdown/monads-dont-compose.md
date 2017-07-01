For a small concrete counterexample, consider the terminal monad.

    data Thud x = Thud

The `return` and `>>=` just go `Thud`, and the laws hold trivially.

Now let's also have the writer monad for Bool (with, let's say, the xor-monoid structure).

    data Flip x = Flip Bool x

    instance Monad Flip where
       return x = Flip False x
       Flip False x  >>= f = f x
       Flip True x   >>= f = Flip (not b) y where Flip b y = f x

Er, um, we'll need composition

    newtype (:.:) f g x = C (f (g x))

Now try to define...

    instance Monad (Flip :.: Thud) where  -- that's effectively the constant `Bool` functor
      return x = C (Flip ??? Thud)
      ...

Parametricity tells us that `???` can't depend in any useful way on `x`, so it must be a constant. As a result, `join . return` is necessarily a constant function also, hence the law

    join . return = id

must fail for whatever definitions of `join` and `return` we choose.
