System F-omega allows universal quantification, abstraction and application at *higher kinds*, so not only over types (at kind `*`), but also at kinds `k1 -> k2`, where `k1` and `k2` are themselves kinds generated from `*` and `->`. Hence, the type level itself becomes a simply typed lambda-calculus.

Haskell delivers slightly less than F-omega, in that the type system allows quantification and application at higher kinds, but not abstraction. Quantification at higher kinds is how we have types like

    fmap :: forall f, s, t. Functor f => (s -> t) -> f s -> f t

with `f :: * -> *`. Correspondingly, variables like `f` can be instantiated with higher-kinded type expressions, such as `Either String`. The lack of abstraction makes it possible to solve unification problems in type expressions by the standard first-order techniques which underpin the Hindley-Milner type system.

However, *type families* are not really another means to introduce higher-kinded types, nor a replacement for the missing type-level lambda. Crucially, they must be *fully applied*. So your example,

    type family Foo a
    type instance Foo Int = Int
    type instance Foo Float = ...
    ....

should not be considered as introducing some

    Foo :: * -> * -- this is not what's happening

because `Foo` on its own is not a meaningful type expression. We have only the weaker rule that `Foo t :: *` whenever `t :: *`.

Type families do, however, act as a distinct type-level computation mechanism beyond F-omega, in that they introduce *equations* between type expressions. The extension of System F with equations is what gives us the "System Fc" which GHC uses today. Equations `s ~ t` between type expressions of kind `*` induce coercions transporting values from `s` to `t`. Computation is done by deducing equations from the rules you give when you define type families.

Moreover, you *can* give type families a higher-kinded return type, as in

    type family Hoo a
    type instance Hoo Int = Maybe
    type instance Hoo Float = IO
    ...

so that `Hoo t :: * -> *` whenever `t :: *`, but still we cannot let `Hoo` stand alone.

The trick we sometimes use to get around this restriction is `newtype` wrapping:

    newtype Noo i = TheNoo {theNoo :: Foo i}

which does indeed give us

    Noo :: * -> *

but means that we have to apply the projection to make computation happen, so `Noo Int` and `Int` are provably distinct types, but

    theNoo :: Noo Int -> Int

So it's a bit clunky, but we can kind of compensate for the fact that type families do not directly correspond to type operators in the F-omega sense.
