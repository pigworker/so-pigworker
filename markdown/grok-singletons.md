What do we gain? Hmm. The status of singletons is that of *awkward but currently necessary workaround*, and the sooner we can do away with them, the better.

Let me see if I can clarify the picture. We have a data type `Nat`:

    data Nat = Zero | Suc Nat

(wars have been started over even more trivial issues than the number of 'c's in `Suc`)

The type `Nat` has run-time values which are indistinguishable at the type level. The Haskell type system currently has the *replacement* property, which means that in any well typed program, you may replace any well typed subexpression by an alternative subexpression with the same scope and type, and the program will continue to be well typed. For example, you can rewrite every occurrence of

    if <b> then <t> else <e>

to

    if <b> then <e> else <t>

and you can be sure that nothing will go wrong...with the outcome of checking your program's type.

The replacement property is an embarrassment. It's clear proof that your type system gives up at the very moment that meaning starts to matter.

Now, by being a data type for run-time values, `Nat` also becomes a type of type-level values `'Zero` and `'Suc`. The latter live only in Haskell's type language and have no run-time presence at all. Please note that although `'Zero` and `'Suc` exist at the type level, it is unhelpful to refer to them as "types" and the people who currently do that should desist. They do not have type `*` and can thus not *classify values* which is what types worthy of the name do.

There is no direct means of exchange between run-time and type-level `Nat`s, which can be a nuisance. The paradigmatic example concerns a key operation on *vectors*:

    data Vec :: Nat -> * -> * where
      VNil   :: Vec 'Zero x
      VCons  :: x -> Vec n x -> Vec ('Suc n) x

We might like to compute a vector of copies of a given element (perhaps as part of an `Applicative` instance). It might look like a good idea to give the type

    vec :: forall (n :: Nat) (x :: *). x -> Vec n x

but can that possibly work? In order to make `n` copies of something, we need to know `n` at run time: a program has to decide whether to deploy `VNil` and stop or to deploy `VCons` and keep going, and it needs some data to do that. A good clue is the `forall` quantifier, which is *parametric*: it indicates thats the quantified information is available only to types and is erased by run time.

Haskell currently enforces an entirely spurious coincidence between dependent quantification (what `forall` does) and erasure for run time. It does *not* support a dependent but not erased quantifier, which we often call `pi`. The type and implementation of `vec` should be something like

    vec :: pi (n :: Nat) -> forall (x :: *). Vec n x
    vec 'Zero    x = VNil
    vec ('Suc n) x = VCons x (vec n x)

where arguments in `pi`-positions are written in the type language, but the data are available at run time.

So what do we do instead? We use singletons to capture indirectly what it means to be a *run-time copy of type-level data*.

    data SNat :: Nat -> * where
      SZero :: SNat Zero
      SSuc  :: SNat n -> SNat (Suc n)

Now, `SZero` and `SSuc` make run-time data. `SNat` is not isomorphic to `Nat`: the former has type `Nat -> *`, while the latter has type `*`, so it is a type error to try to make them isomorphic. There are many run-time values in `Nat`, and the type system does not distinguish them; there is exactly one run-time value (worth speaking of) in each different `SNat n`, so the fact that the type system cannot distinguish them is beside the point. The point is that each `SNat n` is a different type for each different `n`, and that GADT pattern matching (where a pattern can be of a more specific instance of the GADT type it is known to be matching) can refine our knowledge of `n`.

We may now write

    vec :: forall (n :: Nat). SNat n -> forall (x :: *). x -> Vec n x
    vec SZero    x = VNil
    vec (SSuc n) x = VCons x (vec n x)

Singletons allow us to bridge the gap between run time and type-level data, by exploiting the only form of run-time analysis that allows the refinement of type information. It's quite sensible to wonder if they're really necessary, and they presently are, only because that gap has not yet been eliminated.
