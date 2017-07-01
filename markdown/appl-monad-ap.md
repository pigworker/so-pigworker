Perhaps the canonical example is given by the vectors.

    data Nat = Z | S Nat deriving (Show, Eq, Ord)

    data Vec :: Nat -> * -> * where
      V0    ::                  Vec Z x
      (:>)  :: x -> Vec n x ->  Vec (S n) x

We can make them applicative with a little effort, first defining singletons, then wrapping them in a class.

    data Natty :: Nat -> * where
      Zy  :: Natty Z
      Sy  :: Natty n -> Natty (S n)

    class NATTY (n :: Nat) where
      natty :: Natty n

    instance NATTY Z where
      natty = Zy

    instance NATTY n => NATTY (S n) where
      natty = Sy natty

Now we may develop the `Applicative` structure

    instance NATTY n => Applicative (Vec n) where
      pure   = vcopies natty
      (<*>)  = vapp

    vcopies :: forall n x. Natty n -> x -> Vec n x
    vcopies  Zy      x  =  V0
    vcopies  (Sy n)  x  =  x :> vcopies n x   

    vapp :: forall n s t. Vec n (s -> t) -> Vec n s -> Vec n t
    vapp  V0         V0         = V0
    vapp  (f :> fs)  (s :> ss)  = f s :> vapp fs ss

I omit the `Functor` instance (which should be extracted via `fmapDefault` from the `Traversable` instance).

Now, there is a `Monad` instance corresponding to this `Applicative`, but what is it? *Diagonal thinking! That's what's required!* A vector can be seen as the tabulation of a function from a finite domain, hence the `Applicative` is just a tabulation of the K- and S-combinators, and the `Monad` has a `Reader`-like behaviour.

    vtail :: forall n x. Vec (S n) x -> Vec n x
    vtail (x :> xs) = xs

    vjoin :: forall n x. Natty n -> Vec n (Vec n x) -> Vec n x
    vjoin Zy     _                  = V0
    vjoin (Sy n) ((x :> _) :> xxss) = x :> vjoin n (fmap vtail xxss)

    instance NATTY n => Monad (Vec n) where
      return    = vcopies natty
      xs >>= f  = vjoin natty (fmap f xs)

You might save a bit by defining `>>=` more directly, but any way you cut it, the monadic behaviour creates useless thunks for off-diagonal computations. Laziness might save us from slowing down by an armageddon factor, but the zipping behaviour of the `<*>` is bound to be at least a little cheaper than taking the diagonal of a matrix.
