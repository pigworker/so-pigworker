If your program *really* seemed valid to you, then you would be able to write the type of `get` that does the job you want in Haskell, not in handwave. Let me help you improve your handwave and uncover the reason you are asking for the moon on a stick.

> What I want to express is: `get :: (Convert a_contained_by_D b) => D -> b`, which seems impossible.

As stated, that's not quite as precise as you would need. Indeed, it's what Haskell gives you now, in that

    get :: (Convert A b, Convert B b, Convert C b) => D -> b

any `a` which can be contained by `D` is required, one at a time, to be convertible to that `b`. And that's why you're getting classic sysadmin logic: no `D` is allowed to be gotten unless they all can `b`.

The problem is that you need to know the status not of the type which might be contained in *any* old `D`, but rather the type contained in the particular `D` that you receive as the input. Right? You want

    print (get (DB B) :: A)  -- this to work
    print (get (DC C) :: A)  -- this to fail

but `DB B` and `DC C` are just two different elements of `D`, and as far as the Haskell type system is concerned, within each type **everything different is the same**. If you want to discriminate between elements of `D`, then you need a `D`-pendent type. Here's how I'd write it in handwave.

    DInner :: D -> *
    DInner (DA a) = A
    DInner (DB b) = B
    DInner (DC c) = C

    get :: forall x. pi (d :: D) -> (Convert (DInner d) x) => x
    get (DA x) = convert x
    get (DB x) = convert x
    get (DC x) = convert x

where `pi` is the binding form for data which are passed at run time (unlike `forall`) but on which types may depend (unlike `->`). Now the constraint is talking not about arbitrary `D`s but the very `d :: D` in your hand, and the constraint can compute exactly what is needed by inspecting its `DInner`.

There is nothing you can say that will make it go away but my `pi`.

Sadly, whilst `pi` is rapidly descending from the sky, it has not yet landed. None the less, unlike the moon, it can be reached with a stick. No doubt you will complain that I am changing the setup, but really I am just translating your program from Haskell in approximately 2017 to Haskell in 2015. You'll `get` it back, one day, with the very type I handwaved.

There is nothing you can say, but you can *sing*.

Step 1. Switch on `DataKinds` and `KindSignatures` and build the singletons for your types (or get Richard Eisenberg to do it for you).

    data A = A deriving Show
    data Aey :: A -> * where  -- think of "-ey" as an adjectival suffix
      Aey :: Aey 'A           -- as in "tomatoey"

    data B = B deriving Show
    data Bey :: B -> * where
      Bey :: Bey 'B

    data C = C deriving Show
    data Cey :: C -> * where
      Cey :: Cey 'C

    data D = DA A | DB B | DC C deriving Show
    data Dey :: D -> * where
      DAey :: Aey a -> Dey (DA a)
      DBey :: Bey b -> Dey (DB b)
      DCey :: Cey c -> Dey (DC c)

The idea is (i) that datatypes become kinds, and (ii) that singletons characterize the type-level data which have a run time presentation. So type level `DA a` exists at run time provided `a` does, etc.

Step 2. Guess who's coming to `DInner`. Switch on `TypeFamilies`.

    type family DInner (d :: D) :: * where
      DInner (DA a) = A
      DInner (DB b) = B
      DInner (DC c) = C

Step 3. Get you some `RankNTypes`, and now you can write

    get :: forall x. forall d. Dey d -> (Convert (DInner d) x) => x
    --               ^^^^^^^^^^^^^^^^^^
    -- this is a plausible fake of pi (d :: D) ->

Step 4. Try to write `get` and screw up. We have to match on the run time evidence that the type level `d` is representable. We need that to get the type level `d` specialised in the computation of `DInner`. If we had proper `pi`, we could match on a `D` value that serves double duty, but for now, match on `Dey d` instead.

    get (DAey x) = convert x   -- have x :: Aey a, need x :: A
    get (DBey x) = convert x   -- and so on
    get (DCey x) = convert x   -- and so forth

Maddeningly, our `x`es are now singletons, where, to `convert`, we need the underlying data. We need more of the singleton apparatus.

Step 5. Introduce and instantiate the singleton class, to "demote" type level values (as long as we know their run time representatives). Again, Richard Eisenberg's `singletons` library can Template-Haskell the boilerplate out of this, but let's see what's going on

    class Sing (s :: k -> *) where   -- s is the singleton family for some k
      type Sung s :: *               -- Sung s is the type-level version of k
      sung :: s x -> Sung s          -- sung is the demotion function

    instance Sing Aey where
      type Sung Aey = A
      sung Aey = A

    instance Sing Bey where
      type Sung Bey = B
      sung Bey = B

    instance Sing Cey where
      type Sung Cey = C
      sung Cey = C

    instance Sing Dey where
      type Sung Dey = D
      sung (DAey aey) = DA (sung aey)
      sung (DBey bey) = DB (sung bey)
      sung (DCey cey) = DC (sung cey)

Step 6. Do it.

    get :: forall x. forall d. Dey d -> (Convert (DInner d) x) => x
    get (DAey x) = convert (sung x)
    get (DBey x) = convert (sung x)
    get (DCey x) = convert (sung x)

Be assured, when we have proper `pi`, those `DAey`s will be actual `DA`s and those `x`s will no longer need to be `sung`. My handwave type for `get` will be Haskell, and your code for `get` will be fine. But in the meantime

    main = do
      print (get (DCey Cey) :: B)
      print (get (DBey Bey) :: A)

typechecks just fine. That's to say, your program (plus `DInner` and the correct type for `get`) seems like valid Dependent Haskell, and we're nearly there.
