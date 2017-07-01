Expanding on my comment, here's a first crack. The modulus is enforced by type, but not the canonical choice of representative: that's just done by computation, so would necessitate an abstraction barrier. Types of bounded numbers are also available, but they take a bit more work.

Enter, `{-# LANGUAGE KitchenSink #-}`. I mean (actually the not too bad)

    {-# LANGUAGE DataKinds, GADTs, KindSignatures, FlexibleInstances #-}

and let's get cracking.

Firstly, just by reflex, I introduce the Hasochistic natural numbers:

    data Nat = Z | S Nat              -- type-level numbers
    data Natty :: Nat -> * where      -- value-level representation of Nat
      Zy :: Natty Z
      Sy :: Natty n -> Natty (S n)
    class NATTY n where               -- value-level representability
      natty :: Natty n
    instance NATTY Z where
      natty = Zy
    instance NATTY n => NATTY (S n) where
      natty = Sy natty

To my mind, that's just what you do when you want to declare a datatype and then allow other types to depend on its values. Richard Eisenberg's "singletons" library automates the construction.

(If the example goes on to use numbers to index vectors, some people point out that vectors of `()` can also serve as singletons for `Nat`. They're technically correct, of course, but misguided. When we think of `Natty` and `NATTY` as systematically generated from `Nat`, they're an entitlement we can exploit or not as we see fit, not an extra to justify. This example does not involve vectors, and it would be perverse to introduce vectors just to have singletons for `Nat`.)

I hand-roll a bunch of conversion functions and `Show` instances, so we can see what we're doing, apart from anything else.

    int :: Nat -> Integer
    int Z = 0
    int (S n) = 1 + int n

    instance Show Nat where
      show = show . int

    nat :: Natty n -> Nat
    nat Zy = Z
    nat (Sy n) = S (nat n)

    instance Show (Natty n) where
      show = show . nat

Now we're ready to declare `Mod`.

    data Mod :: Nat -> * where
      (:%) :: Integer -> Natty n -> Mod (S n)

The type carries the modulus. The values carry an unnormalized representative of the equivalence class, but we had better figure out how to normalize it. Division for unary numbers is a peculiar sport which I learned as a child.

    remainder :: Natty n   -- predecessor of modulus
              -> Integer   -- any representative
              -> Integer   -- canonical representative
      -- if candidate negative, add the modulus
    remainder n x | x < 0 = remainder n (int (nat (Sy n)) + x)
      -- otherwise get dividing
    remainder n x = go (Sy n) x x where
      go :: Natty m  -- divisor countdown (initially the modulus)
         -> Integer  -- our current guess at the representative
         -> Integer  -- dividend countdown
         -> Integer  -- the canonical representative
        -- when we run out of dividend the guessed representative is canonical
      go _      c 0 = c
        -- when we run out of divisor but not dividend,
        --   the current dividend countdown is a better guess at the rep,
        --   but perhaps still too big, so start again, counting down
        --   from the modulus (conveniently still in scope)
      go Zy     _ y = go (Sy n) y y
        -- otherwise, decrement both countdowns
      go (Sy m) c y = go m c (y - 1)

Now we can make a smart constructor.

    rep :: NATTY n                 -- we pluck the modulus rep from thin air
        => Integer -> Mod (S n)    -- when we see the modulus we want
    rep x = remainder n x :% n where n = natty

And then the `Monoid` instance is easy:

    instance NATTY n => Monoid (Mod (S n)) where
      mempty                    = rep 0
      mappend (x :% _) (y :% _) = rep (x + y)

I chucked in some other things, too:

    instance Show (Mod n) where
      show (x :% n) = concat ["(", show (remainder n x), " :% ", show (Sy n), ")"]
    instance Eq (Mod n) where
      (x :% n) == (y :% _) = remainder n x == remainder n y

With a little convenience...

    type Four = S (S (S (S Z)))

we get

    > foldMap rep [1..5] :: Mod Four
    (3 :% 4)

So yes, you do need dependent types, but Haskell is dependently typed enough.
