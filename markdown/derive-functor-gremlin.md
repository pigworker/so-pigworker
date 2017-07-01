Working in ghci, if you make the incantation

    Prelude> :set -XDeriveFunctor

then the compiler will become as clever as you are hoping for, if not quite as enthusiastic. You will need to invoke the functionality, thus,

    Prelude> data Foo a = Foo a deriving (Show, Functor)

(the `Show` is just for printing the output, below) and then you will be able to do stuff like

    Prelude> fmap (++"bar") (Foo "foo")
    Foo "foobar"

In a module, you achieve the same by adding the pragma

    {-# LANGUAGE DeriveFunctor #-}

before the `module` declaration. It's good for at least the more straightforward `Functor` instances, but you can fool it into a false negative.

    Prelude> data Boo a = Boo (Either a Bool) deriving Functor

    <interactive>:9:43:
        Can't make a derived instance of ‘Functor Boo’:
          Constructor ‘Boo’ must use the type variable only as the
            last argument of a data type
          In the data declaration for ‘Boo’

Meanwhile

    data Goo a = Goo (Either Bool a) deriving Functor

is ok, and the machinery has clearly been hacked to work with pairing, as

    data Woo a = Woo (a, Bool) deriving Functor

is permitted.

So it's not as clever as it could be, but it's better than a poke in the eye.
