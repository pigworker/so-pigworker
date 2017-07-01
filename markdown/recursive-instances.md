The definition of `pure` is indeed at the heart of the problem. What should its type be, fully quantified and qualified?

    pure :: forall (n :: Nat) (x :: *). x -> Vector n x            -- (X)

won't do, as there is no information available at run-time to determine whether `pure` should emit `VNil` or `VCons`. Correspondingly, as things stand, you can't just have

    instance Applicative (Vector n)                                -- (X)

What can you do? Well, working with the [Strathclyde Haskell Enhancement][1], in the [Vec.lhs][2] example file, I define a precursor to `pure`

    vec :: forall x. pi (n :: Nat). x -> Vector {n} x
    vec {Zero}    x = VNil
    vec {Succ n}  x = VCons x (vec n x)

with a `pi` type, requiring that a copy of `n` be passed at runtime. This `pi (n :: Nat).` desugars as

    forall n. Natty n ->

where `Natty`, with a more prosaic name in real life, is the singleton GADT given by

    data Natty n where
      Zeroy :: Natty Zero
      Succy :: Natty n -> Natty (Succ n)

and the curly braces in the equations for `vec` just translate `Nat` constructors to `Natty` constructors. I then define the following diabolical instance (switching off the default Functor instance)

    instance {:n :: Nat:} => Applicative (Vec {n}) where
      hiding instance Functor
      pure = vec {:n :: Nat:} where
      (<*>) = vapp where
        vapp :: Vec {m} (s -> t) -> Vec {m} s -> Vec {m} t
        vapp  VNil          VNil          = VNil
        vapp  (VCons f fs)  (VCons s ss)  = VCons (f s) vapp fs ss

which demands further technology, still. The constraint `{:n :: Nat:}` desugars to something which requires that a `Natty n` witness exists, and by the power of scoped type variables, the same `{:n :: Nat:}` subpoenas that witness explicitly. Longhand, that's

    class HasNatty n where
      natty :: Natty n
    instance HasNatty Zero where
      natty = Zeroy
    instance HasNatty n => HasNatty (Succ n) where
      natty = Succy natty

and we replace the constraint `{:n :: Nat:}` with `HasNatty n` and the corresponding term with `(natty :: Natty n)`. Doing this construction systematically amounts to writing a fragment of a Haskell typechecker in type class Prolog, which is not my idea of joy so I use a computer.

Note that the `Traversable` instance (pardon my idiom brackets and my silent default Functor and Foldable instances) requires no such jiggery pokery 

    instance Traversable (Vector n) where
      traverse f VNil         = (|VNil|)
      traverse f (VCons x xs) = (|VCons (f x) (traverse f xs)|)

That's all the structure you need to get matrix multiplication without further explicit recursion.

**TL;DR Use the singleton construction and its associated type class to collapse all of the recursively defined instances into the existence of a runtime witness for the type-level data, from which you can compute by explicit recursion.**

What are the design implications?

GHC 7.4 has the type promotion technology but SHE still has the singleton construction `pi`-types to offer. One clearly important thing about promoted datatypes is that they're *closed*, but that isn't really showing up cleanly yet: the constructability of singleton witnesses is the manifestation of that closedness. Somehow, if you have `forall (n :: Nat).` then it's always reasonable to demand a singleton as well, but to do so makes a difference to the generated code: whether it's explicit as in my `pi` construct, or implicit as in the dictionary for `{:n :: Nat:}`, there is extra runtime information to sling around, and a correspondingly weaker free theorem.

An open design question for future versions of GHC is how to manage this distinction between the presence and absence of runtime witnesses to type-level data. On the one hand, we need them in constraints. On the other hand, we need to pattern-match on them. E.g., should `pi (n :: Nat).` mean the explicit

    forall (n :: Nat). Natty n ->

or the implicit

    forall (n :: Nat). {:n :: Nat:} =>

? Of course, languages like Agda and Coq have both forms, so maybe Haskell should follow suit. There is certainly room to make progress, and we're working on it!


  [1]: http://personal.cis.strath.ac.uk/~conor/pub/she
  [2]: https://personal.cis.strath.ac.uk/~conor/pub/she/examples/Vec.lhs
