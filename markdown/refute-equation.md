Here's a shorter version of Philip JF's solution, which is the way dependent type theorists have been refuting equations for years.

    type family Discriminate x
    type instance Discriminate Int  = ()
    type instance Discriminate Char = Void

    transport :: Equal a b -> Discriminate a -> Discriminate b
    transport Refl d = d

    refute :: Equal Int Char -> Void
    refute q = transport q ()

In order to show that things are different, you have to catch them *behaving differently* by providing a computational context which results in distinct observations. `Discriminate` provides exactly such a context: a type-level program which treats the two types differently.

It is not necessary to resort to `undefined` to solve this problem. Total programming sometimes involves rejecting impossible inputs. Even where `undefined` is available, I would recommend not using it where a total method suffices: the total method *explains* why something is impossible and the typechecker confirms; `undefined` merely documents *your promise*. Indeed, this method of refutation is how Epigram dispenses with "impossible cases" whilst ensuring that a case analysis covers its domain.

As for computational behaviour, note that `refute`, via `transport` is necessarily strict in `q` and that `q` cannot compute to head normal form in the empty context, simply because no such head normal form exists (and because computation preserves type, of course). In a total setting, we'd be sure that `refute` would never be invoked at run time. In Haskell, we're at least certain that its argument will diverge or throw an exception before we're obliged to respond to it. A *lazy* version, such as

    absurdEquality e = error "you have a type error likely to cause big problems"

will ignore the toxicity of `e` and tell you that you have a type error when you don't. I prefer

    absurdEquality e = e `seq` error "sue me if this happens"

if the honest refutation is too much like hard work.
