The thing that you ask for is not possible, but something quite like it might do instead. It's not possible because the proof requires a case analysis on type level Booleans, but you have no data which enables you to make such an event occur. The fix is to include just such information via a singleton.

First up, let me note that your type for `idemp` is a little obfuscated. The constraint `a ~ b` just names the same thing twice. The following typechecks:

    idemq :: p (Or b b) -> p b
    idemq = undefined
    idemp :: a ~ b => p (Or a b) -> p a
    idemp = idemq

(If you have a constraint `a ~ t` where `t` does not contain `a`, it's usually good to substitute `t` for `a`. The exception is in `instance` declarations: an `a` in an instance head will match anything, hence the instance will fire even if that thing has not yet obviously become `t`. But I digress.)

I claim `idemq` is undefinable because we have no useful information about `b`. The only data available inhabit `p`-of-something, and we don't know what `p` is.

We need to reason by cases on `b`. Bear in mind that with general recursive type families, we can define type level Booleans which are neither `True` nor `False`. If I switch on `UndecidableInstances`, I can define

    type family Loop (b :: Bool) :: Bool
    type instance Loop True = Loop False
    type instance Loop False = Loop True

so `Loop True` cannot be reduced to `True` or `False`, and locally worse, there is no way to show that

    Or (Loop True) (Loop True) ~ Loop True     -- this ain't so

There's no way out of it. We need run time evidence that our `b` is one of the well behaved Booleans that computes somehow to a value. Let us therefore *sing*

    data Booly :: Bool -> * where
      Truey   :: Booly True
      Falsey  :: Booly False

If we know `Booly b`, we can do a case analysis which will tell us what `b` is. Each case will then go through nicely. Here's how I'd play it, using an equality type defined with `PolyKinds` to pack up the facts, rather than abstracting over uses `p b`.

    data (:=:) a b where
      Refl :: a :=: a

Our key fact is now plainly stated and proven:

    orIdem :: Booly b -> Or b b :=: b
    orIdem Truey   = Refl
    orIdem Falsey  = Refl

And we can deploy this fact by strict case analysis:

    idemp :: Booly b -> p (Or b b) -> p b
    idemp b p = case orIdem b of Refl -> p

The case analysis must be strict, to check that the evidence is not some loopy lie, but rather an honest to goodness `Refl` silently packing up just the proof of `Or b b ~ b` that's needed to fix up the types.

If you don't want to sling all these singleton values around explicitly, you can, as kosmikus suggests, hide them in a dictionary and extract them just when you need them.

Richard Eisenberg and Stephanie Weirich have a Template Haskell library which mills these families and classes for you. SHE can build them too and lets you write

    orIdem pi b :: Bool. Or b b :=: b
    orIdem {True}   = Refl
    orIdem {False}  = Refl

where `pi b :: Bool.` expands to `forall b :: Bool. Booly b ->`.

But it's such a palaver. That's why my gang are working on adding an actual `pi` to Haskell, being a non-parametric quantifier (distinct from `forall` and `->`) which can be instantiated by stuff in the now nontrivial intersection between Haskell's type and term languages. This `pi` could also have an "implicit" variant, where the argument is by default kept hidden. The two respectively correspond to using singleton families and classes, but there's no need to define datatypes three times over to get the additional kit.

It might be worth mentioning that in a total type theory, it is *not* needed to pass the extra copy of the Boolean `b` at run time. The thing is, `b` is used only to make the proof that data may be transported from `p (Or b b)` to `p b`, not necessarily to make the data being transported. We don't compute under binders at run time, so there's no way to cook up a dishonest proof of the equation, hence we can erase the proof component and the copy of `b` that delivers it. As Randy Pollack says, *the best thing about working in a strongly normalizing calculus is not having to normalize things*.
