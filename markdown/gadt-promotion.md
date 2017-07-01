An interesting thing happens if you promote types indexed by promoted types. Imagine we build

    data Nat = Ze | Su Nat

and then

    data Vec :: Nat -> * -> * where
      VNil   :: Vec Ze x
      VCons  :: x -> Vec n x -> Vec (Su n) x

Behind the scenes, the *internal* types of the constructors represent the instantiated return indices by constraints, as if we had written

    data Vec (n :: Nat) (a :: *)
      =            n ~ Ze    => VNil
      | forall k.  n ~ Su k  => VCons a (Vec k a)

Now if we were allowed something like

    data Elem :: forall n a. a -> Vec n a -> * where
      Top :: Elem x (VCons x xs)
      Pop :: Elem x xs -> Elem x (VCons y xs)

the translation to internal form would have to be something like

    data Elem (x :: a) (zs :: Vec n a)
      = forall (k :: Nat), (xs :: Vec k a).            (n ~ Su k, zs ~ VCons x xs) =>
          Top
      | forall (k :: Nat), (xs :: Vec k s), (y :: a).  (n ~ Su k, zs ~ VCons y xs) =>
          Pop (Elem x xs)

but look at the second constraint in each case! We have

    zs :: Vec n a

but

    VCons x xs, VCons y xs :: Vec (Su k) a

But in System FC as then defined, equality constraints must have types of the same kind on both sides, so this example is not inconsiderably problematic.

One fix is use the evidence for the first constraint to fix up the second, but then we'd need dependent constraints

    (q1 :: n ~ Su k, zs |> q1 ~ VCons x xs)

Another fix is just to allow heterogeneous equations, as I did in dependent type theory fifteen years ago. There will inevitably be equations between things whose kinds are equal in ways which are not syntactically obvious.

It's the latter plan that is currently favoured. As far as I understand, the policy you mention was adopted as a holding position, until the design for a core language with heterogeneous equality (as proposed by Weirich and colleagues) has matured to implementation. We live in interesting times.
