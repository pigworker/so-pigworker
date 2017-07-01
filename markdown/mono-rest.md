There's another factor here, mentioned in some of the links which acfoltzer includes, but it might be worth making explicit here. You're encountering the effect of the [monomorphism restriction][1]. When you say

    let x = 5

you make a *top-level* definition of a *variable*. The MR insists that such definitions, when otherwise unaccompanied by a type signature, should be specialized to a monomorphic value by choosing (hopefully) suitable default instances for the unresolved type variables. By contrast, when you use `:t` to ask for an inferred type, no such restriction or defaulting is imposed. So

    > :t 3
    3 :: (Num t) => t

because `3` is indeed overloaded: it is admitted by any numeric type. The defaulting rules choose `Integer` as the default numeric type, so

    > let x = 3
    > :t x
    x :: Integer

But now let's switch off the MR.

    > :set -XNoMonomorphismRestriction
    > let y = 3
    > :t y
    y :: (Num t) => t

Without the MR, the definition is just as polymorphic as it can be, just as overloaded as `3`. Just checking...

    > :t y * (2.5 :: Float)
    y * (2.5 :: Float) :: Float
    > :t y * (3 :: Int)
    y * (3 :: Int) :: Int

Note that the polymorphic `y = 3` is being differently specialized in these uses, according to the `fromInteger` method supplied with the relevant `Num` instance. That is, `y` is not associated with a particular representation of `3`, but rather a scheme for constructing representations of `3`. Naïvely compiled, that's a recipe for slow, which some people cite as a motivation for the MR.

I'm (locally pretending to be) neutral on the debate about whether the monomorphism restriction is a lesser or greater evil. I always write type signatures for top-level definitions, so there is no ambiguity about what I'm trying to achieve and the MR is beside the point.

When trying to learn how the type system works, it's really useful to separate the aspects of type inference which

 1. &lsquo;follow the plan&rsquo;, specializing polymorphic definitions to particular use cases: a fairly robust matter of constraint-solving, requiring basic unification and instance resolution by backchaining; and

 2. &lsquo;guess the plan&rsquo;, generalizing types to assign a polymorphic type scheme to a definition with no type signature: that's quite fragile, and the more you move past the basic Hindley-Milner discipline, with type classes, with higher-rank polymorphism, with GADTs, the stranger things become.

It's good to learn how the first works, and to understand why the second is difficult. Much of the weirdness in type inference is associated with the second, and with heuristics like the monomorphism restriction trying to deliver useful default behaviour in the face of ambiguity.

  [1]: http://www.haskell.org/haskellwiki/Monomorphism_restriction
