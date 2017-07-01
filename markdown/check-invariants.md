The following is a stunt, but it's quite a safe stunt so do try it at home. It uses some of the entertaining new toys to bake *order* invariants into mergeSort.

    {-# LANGUAGE GADTs, PolyKinds, KindSignatures, MultiParamTypeClasses,
        FlexibleInstances, RankNTypes, FlexibleContexts #-}

I'll have natural numbers, just to keep things simple.

    data Nat = Z | S Nat deriving (Show, Eq, Ord)

But I'll define `<=` in type class Prolog, so the typechecker can try to figure order out implicitly.

    class LeN (m :: Nat) (n :: Nat) where
    instance             LeN Z n where
    instance LeN m n =>  LeN (S m) (S n) where

In order to sort numbers, I need to know that any two numbers can be ordered *one way or the other*. Let's say what it means for two numbers to be so orderable.

    data OWOTO :: Nat -> Nat -> * where
      LE :: LeN x y => OWOTO x y
      GE :: LeN y x => OWOTO x y

We'd like to know that every two numbers are indeed orderable, provided we have a runtime representation of them. These days, we get that by building the *singleton family* for `Nat`. `Natty n` is the type of runtime copies of `n`.

    data Natty :: Nat -> * where
      Zy :: Natty Z
      Sy :: Natty n -> Natty (S n)

Testing which way around the numbers are is quite a lot like the usual Boolean version, except with evidence. The step case requires unpacking and repacking because the types change. Instance inference is good for the logic involved.

    owoto :: forall m n. Natty m -> Natty n -> OWOTO m n
    owoto Zy      n       = LE
    owoto (Sy m)  Zy      = GE
    owoto (Sy m)  (Sy n)  = case owoto m n of
      LE -> LE
      GE -> GE

Now we know how to put numbers in order, let's see how to make ordered lists. The plan is to describe what it is to be in order *between loose bounds*. Of course, we don't want to exclude any elements from being sortable, so the type of *bounds* extends the element type with bottom and top elements.

    data Bound x = Bot | Val x | Top deriving (Show, Eq, Ord)

I extend the notion of `<=` accordingly, so the typechecker can do bound checking.

    class LeB (a :: Bound Nat)(b :: Bound Nat) where
    instance             LeB Bot     b        where
    instance LeN x y =>  LeB (Val x) (Val y)  where
    instance             LeB (Val x) Top      where
    instance             LeB Top     Top      where

And here are ordered lists of numbers: an `OList l u` is a sequence `x1 :< x2 :< ... :< xn :< ONil` such that `l <= x1 <= x2 <= ... <= xn <= u`. The `x :<` checks that `x` is above the lower bound, then imposes `x` as the lower bound on the tail.

    data OList :: Bound Nat -> Bound Nat -> * where
      ONil :: LeB l u => OList l u
      (:<) :: forall l x u. LeB l (Val x) =>
              Natty x -> OList (Val x) u -> OList l u

We can write `merge` for ordered lists just the same way we would if they were ordinary. The key invariant is that if both lists share the same bounds, so does their merge.

    merge :: OList l u -> OList l u -> OList l u
    merge ONil      lu         = lu
    merge lu        ONil       = lu
    merge (x :< xu) (y :< yu)  = case owoto x y of
      LE  -> x :< merge xu (y :< yu)
      GE  -> y :< merge (x :< xu) yu

The branches of the case analysis extend what is already known from the inputs with just enough ordering information to satisfy the requirements for the results. Instance inference acts as a basic theorem prover: fortunately (or rather, with a bit of practice) the proof obligations are easy enough.

Let's seal the deal. We need to construct runtime witnesses for numbers in order to sort them this
way.

    data NATTY :: * where
      Nat :: Natty n -> NATTY

    natty :: Nat -> NATTY
    natty Z      =                           Nat Zy
    natty (S n)  = case natty n of Nat n ->  Nat (Sy n)

We need to trust that this translation gives us the `NATTY` that corresponds to the `Nat` we want to sort. This interplay between `Nat`, `Natty` and `NATTY` is a bit frustrating, but that's what it takes in Haskell just now. Once we've got that, we can build `sort` in the usual divide-and-conquer way.

    deal :: [x] -> ([x], [x])
    deal []        = ([], [])
    deal (x : xs)  = (x : zs, ys) where (ys, zs) = deal xs

    sort :: [Nat] -> OList Bot Top
    sort []   = ONil
    sort [n]  = case natty n of Nat n -> n :< ONil
    sort xs   = merge (sort ys) (sort zs) where (ys, zs) = deal xs

I'm often surprised by how many programs that make sense to us can make just as much sense to a typechecker.

[Here's some spare kit I built to see what was happening.

    instance Show (Natty n) where
      show Zy = "Zy"
      show (Sy n) = "(Sy " ++ show n ++ ")"
    instance Show (OList l u) where
      show ONil = "ONil"
      show (x :< xs) = show x ++ " :< " ++ show xs
    ni :: Int -> Nat
    ni 0 = Z
    ni x = S (ni (x - 1))

And nothing was hidden.]
