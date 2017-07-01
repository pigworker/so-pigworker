Here's my version of your program. I'm using

    {-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeFamilies #-}

and I've got `Nat` and its singleton

    data Nat = Z | S Nat

    data SNat :: Nat -> * where
      ZZ :: SNat Z
      SS :: SNat n -> SNat (S n)

Your `Interval` type is more familiar to me as the "suffix" definition of "less-than-or-equal": "suffix" because if you upgraded from numbers to lists and labelled each `S` with an element, you'd have the definition of a list suffix.

    data Le :: Nat -> Nat -> * where
      Len :: SNat n -> Le n n
      Les :: Le m n -> Le m (S n)

Here's addition.

    type family Plus (x :: Nat) (y :: Nat) :: Nat
    type instance Plus Z     y  = y
    type instance Plus (S x) y  = S (Plus x y)

Now, your puzzle is to count the `Les` constructors in some `Le`-value, extracting the singleton for the difference between its indices. Rather than *assuming* that we're working with some `Le n (Plus m n)` and trying to compute a `SNat m`, I'm going to write a function which computes the difference between *arbitrary* `Le m o`-indices and *establishes* the connection with `Plus`.

Here's the additive definition of `Le`, with singletons supplied.

    data AddOn :: Nat -> Nat -> * where
      AddOn :: SNat n -> SNat m -> AddOn n (Plus m n)

We can easily establish that `Le` implies `AddOn`. Pattern matching on some `AddOn n o` reveals `o` to be `Plus m n` for some `m` and hands us the singletons we wanted.

    leAddOn :: Le m o -> AddOn m o
    leAddOn (Len n) = AddOn n ZZ
    leAddOn (Les p) = case leAddOn p of AddOn n m -> AddOn n (SS m)

More generally, I'd advise formulating dependently typed programming problems minimizing the presence of defined functions in the indices of types over which you plan to match. This avoids complicated unification. (Epigram used to colour such functions green, hence the advice **"Don't touch the green slime!"**.) `Le n o`, it turns out (for that is the point of `leAddOn`), is no less informative a type than `Le n (Plus m n)`, but it is rather easier to match on.

Yet more generally, it is quite a normal experience to set up a dependent datatype which captures the logic of your problem but is absolutely ghastly to work with. This does not mean that all datatypes which capture the correct logic will be absolutely ghastly to work with, just that you need think harder about the ergonomics of your definition. Getting these definitions neat is not a skill that very many people pick up in their ordinary Functional Programming learning experience, so expect to climb a new learning curve.
