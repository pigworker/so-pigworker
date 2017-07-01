My style may be cramped by my phone, but here goes.

    newtype Not x = Kill {kill :: x -> Void}

cannot be a Functor. If it were, we'd have

    kill (fmap (const ()) (Kill id)) () :: Void

and the Moon would be made of green cheese.

Meanwhile

    newtype Dead x = Oops {oops :: Void}

is a functor

    instance Functor Dead where
      fmap f (Oops corpse) = Oops corpse

but cannot be applicative, or we'd have

    oops (pure ()) :: Void

and Green would be made of Moon cheese (which can actually happen, but only later in the evening).

(Extra note: `Void`, as in `Data.Void` is an empty datatype. If you try to use `undefined` to prove it's a Monoid, I'll use `unsafeCoerce` to prove that it isn't.)

Joyously,

    newtype Boo x = Boo {boo :: Bool}

is applicative in many ways, e.g., as Dijkstra would have it,

    instance Applicative Boo where
      pure _ = Boo True
      Boo b1 <*> Boo b2 = Boo (b1 == b2)

but it cannot be a Monad. To see why not, observe that return must be constantly `Boo True` or `Boo False`, and hence that

    join . return == id

cannot possibly hold.

Oh yeah, I nearly forgot

    newtype Thud x = The {only :: ()}

is a Monad. Roll your own.

Plane to catch... 
