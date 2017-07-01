Yes, you're right to observe that the types versus values distinction in Idris does not align with the compiletime-only versus runtime-and-compiletime distinction. That's a good thing. It is useful to have values which exist only at compiletime, just as in program logics we have "ghost variables" used only in specifications. It is useful also to have type representations at runtime, allowing datatype generic programming.

In Haskell, `DataKinds` (and `PolyKinds`) let us write

    type family Cond (b :: Bool)(t :: k)(e :: k) :: k where
      Cond 'True  t e = t
      Cond 'False t e = e

and in the not too distant future, we shall be able to write

    item :: pi (b :: Bool) -> Cond b Int [Int]
    item True  = 42
    item False = [1,2,3]

but until that technology is implemented, we have to make do with singleton forgeries of dependent function types, like this:

    data Booly :: Bool -> * where
      Truey  :: Booly 'True
      Falsey :: Booly 'False

    item :: forall b. Booly b -> Cond b Int [Int]
    item Truey  = 42
    item Falsey = [1,2,3]

You can get quite far with such fakery, but it would all get a lot easier if we just had the real thing.

Crucially, the plan for Haskell is to maintain and separate `forall` and `pi`, supporting parametric and ad hoc polymorphism, respectively. The lambdas and applications that go with `forall` can still be erased in runtime code generation, just as now, but those for `pi` are retained. It would also make sense to have runtime type abstractions `pi x :: * -> ...` and throw the rats' nest of complexity that is `Data.Typeable` into the dustbin.
