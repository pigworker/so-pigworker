###Alignment###

Dependently typed programming is like doing two jigsaws which some rogue has glued together. Less metaphorically, we express simultaneous computations at the value level and at the type level, and we must ensure their compatibility. Of course, we are each our own rogue, so if we can arrange for the jigsaws to be glued in alignment, we shall have an easier time of it. When you see proof obligations for type repair, you might be tempted to ask

> Do I need to add some sort of proof objects (`data Refl a b where Refl :: Refl a a` et al.) or is there some way to make this work with just adding more explicit type signatures?

But you might first consider in what way the value- and type-level computations are out of alignment, and whether there is any hope to bring them closer.

###A Solution###

The question here is how to compute the vector (length-indexed list) of selections from a vector. So we'd like something with type

    List (Succ n) a -> List (Succ n) (a, List n a)

where the element in each input position gets decorated with the one-shorter vector of its siblings. The proposed method is to scan left-to-right, accumulating the elder siblings in a list which grows on the right, then concatenate with the younger siblings at each position. Growing lists on the right is always a worry, especially when the `Succ` for the length is aligned to the `Cons` on the left. The need for concatenation necessitates type-level addition, but the arithmetic resulting from right-ended activity is out of alignment with the computation rules for addition. I'll get back to this style in a bit, but let's try thinking it out again.

Before we get into any accumulator-based solution, let's just try bog standard structural recursion. We have the "one" case and the "more" case.

    picks (Cons x xs@Nil)         = Cons (x, xs) Nil
    picks (Cons x xs@(Cons _ _))  = Cons (x, xs) (undefined (picks xs))

In both cases, we put the first decomposition at the front. In the second case, we have checked that the tail is nonempty, so we can ask for its selections. We have

    x         :: a
    xs        :: List (Succ n) a
    picks xs  :: List (Succ n) (a, List n a)

and we want

    Cons (x, xs) (undefined (picks xs))  :: List (Succ (Succ n)) (a, List (Succ n) a)
                  undefined (picks xs)   :: List (Succ n) (a, List (Succ n) a)

so the `undefined` needs to be a function which grows all the sibling lists by reattaching `x` at the left end (and left-endedness is good). So, I define the `Functor` instance for `List n`

    instance Functor (List n) where
      fmap f Nil          = Nil
      fmap f (Cons x xs)  = Cons (f x) (fmap f xs)

and I curse the `Prelude` and

    import Control.Arrow((***))

so that I can write

    picks (Cons x xs@Nil)         = Cons (x, xs) Nil
    picks (Cons x xs@(Cons _ _))  = Cons (x, xs) (fmap (id *** Cons x) (picks xs))

which does the job with not a hint of addition, let alone a proof about it.

###Variations###

I got annoyed about doing the same thing in both lines, so I tried to wriggle out of it:

    picks :: m ~ Succ n => List m a -> List m (a, List n a)  -- DOESN'T TYPECHECK
    picks Nil          = Nil
    picks (Cons x xs)  = Cons (x, xs) (fmap (id *** (Cons x)) (picks xs))

But GHC solves the constraint aggressively and refuses to allow `Nil` as a pattern. And it's correct to do so: we really shouldn't be computing in a situation where we know statically that `Zero ~ Succ n`, as we can easily construct some segfaulting thing. The trouble is just that I put my constraint in a place with too global a scope.

Instead, I can declare a wrapper for the result type.

    data Pick :: Nat -> * -> * where
      Pick :: {unpick :: (a, List n a)} -> Pick (Succ n) a

The `Succ n` return index means the nonemptiness constraint is *local* to a `Pick`. A helper function does the left-end extension,

    pCons :: a -> Pick n a -> Pick (Succ n) a
    pCons b (Pick (a, as)) = Pick (a, Cons b as)

leaving us with

    picks' :: List m a -> List m (Pick m a)
    picks' Nil          = Nil
    picks' (Cons x xs)  = Cons (Pick (x, xs)) (fmap (pCons x) (picks' xs))

and if we want

    picks = fmap unpick . picks'

That's perhaps overkill, but it might be worth it if we want to separate older and younger siblings, splitting lists in three, like this:

    data Pick3 :: Nat -> * -> * where
      Pick3 :: List m a -> a -> List n a -> Pick3 (Succ (m + n)) a

    pCons3 :: a -> Pick3 n a -> Pick3 (Succ n) a
    pCons3 b (Pick3 bs x as) = Pick3 (Cons b bs) x as

    picks3 :: List m a -> List m (Pick3 m a)
    picks3 Nil          = Nil
    picks3 (Cons x xs)  = Cons (Pick3 Nil x xs) (fmap (pCons3 x) (picks3 xs))

Again, all the action is left-ended, so we're fitting nicely with the computational behaviour of `+`.

###Accumulating###

If we want to keep the style of the original attempt, accumulating the elder siblings as we go, we could do worse than to keep them *zipper-style*, storing the closest element in the most accessible place. That is, we can store the elder siblings in reverse order, so that at each step we need only `Cons`, rather than concatenating. When we want to build the full sibling list in each place, we need to use reverse-concatenation (really, plugging a sublist into a list zipper). You can type `revCat` easily for vectors if you deploy the *abacus-style* addition:

    type family (+/) (a :: Nat) (b :: Nat) :: Nat
    type instance (+/) Zero     n  =  n
    type instance (+/) (Succ m) n  =  m +/ Succ n

That's the addition which is in alignment with the value-level computation in `revCat`, defined thus:

    revCat :: List m a -> List n a -> List (m +/ n) a
    revCat Nil         ys  =  ys
    revCat (Cons x xs) ys  =  revCat xs (Cons x ys)

We acquire a zipperized `go` version

    picksr :: List (Succ n) a -> List (Succ n) (a, List n a)
    picksr = go Nil where
      go :: List p a -> List (Succ q) a -> List (Succ q) (a, List (p +/ q) a)
      go p (Cons x xs@Nil)         =  Cons (x, revCat p xs) Nil
      go p (Cons x xs@(Cons _ _))  =  Cons (x, revCat p xs) (go (Cons x p) xs)

and nobody proved anything.

###Conclusion###

Leopold Kronecker should have said

> God made the natural numbers *to perplex us*: all the rest is the work of man.

One `Succ` looks very like another, so it is very easy to write down expressions which give the size of things in a way which is out of alignment with their structure. Of course, we can and should (and are about to) equip GHC's constraint solver with improved kit for type-level numerical reasoning. But before that kicks in, it's worth just conspiring to align the `Cons`es with the `Succ`s.
