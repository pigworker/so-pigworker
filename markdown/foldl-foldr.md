**Every `foldl` is a `foldr`.**

Let's remember the definitions.

    foldr :: (a -> s -> s) -> s -> [a] -> s
    foldr f s []       = s
    foldr f s (a : as) = f a (foldr f s as)

That's the standard issue one-step iterator for lists. I used to get my students to bang on the tables and chant "What do you do with the empty list? What do you do with `a : as`"? And that's how you figure out what `s` and `f` are, respectively.

If you think about what's happening, you see that `foldr` effectively computes a big composition of `f a` functions, then applies that composition to `s`.

    foldr f s [1, 2, 3]
    = f 1 . f 2 . f 3 . id $ s

Now, let's check out `foldl`

    foldl :: (t -> a -> t) -> t -> [a] -> t
    foldl g t []       = t
    foldl g t (a : as) = foldl g (g t a) as

That's also a one-step iteration over a list, but with an accumulator which changes as we go. Let's move it last, so that everything to the left of the list argument stays the same.

    flip . foldl :: (t -> a -> t) -> [a] -> t -> t
    flip (foldl g) []       t = t
    flip (foldl g) (a : as) t = flip (foldl g) as (g t a)

Now we can see the one-step iteration if we move the `=` one place leftward.

    flip . foldl :: (t -> a -> t) -> [a] -> t -> t
    flip (foldl g) []       = \ t -> t
    flip (foldl g) (a : as) = \ t -> flip (foldl g) as (g t a)

In each case, we compute *what we would do if we knew the accumulator*, abstracted with `\ t ->`. For `[]`, we would return `t`. For `a : as`, we would process the tail with `g t a` as the accumulator.

But now we can transform `flip (foldl g)` into a `foldr`. Abstract out the recursive call.

    flip . foldl :: (t -> a -> t) -> [a] -> t -> t
    flip (foldl g) []       = \ t -> t
    flip (foldl g) (a : as) = \ t -> s (g t a)
      where s = flip (foldl g) as

And now we're good to turn it into a `foldr` where type `s` is instantiated with `t -> t`.

    flip . foldl :: (t -> a -> t) -> [a] -> t -> t
    flip (foldl g) = foldr (\ a s -> \ t -> s (g t a)) (\ t -> t)

So `s` says "what `as` would do with the accumulator" and we give back `\ t -> s (g t a)` which is "what `a : as` does with the accumulator". Flip back.

    foldl :: (t -> a -> t) -> t -> [a] -> t
    foldl g = flip (foldr (\ a s -> \ t -> s (g t a)) (\ t -> t))

Eta-expand.

    foldl :: (t -> a -> t) -> t -> [a] -> t
    foldl g t as = flip (foldr (\ a s -> \ t -> s (g t a)) (\ t -> t)) t as

Reduce the `flip`.

    foldl :: (t -> a -> t) -> t -> [a] -> t
    foldl g t as = foldr (\ a s -> \ t -> s (g t a)) (\ t -> t) as t

So we compute "what we'd do if we knew the accumulator", and then we feed it the initial accumulator.

It's moderately instructive to golf that down a little. We can get rid of `\ t ->`.

    foldl :: (t -> a -> t) -> t -> [a] -> t
    foldl g t as = foldr (\ a s -> s . (`g` a)) id as t

Now let me reverse that composition using `>>>` from `Control.Arrow`.

    foldl :: (t -> a -> t) -> t -> [a] -> t
    foldl g t as = foldr (\ a s -> (`g` a) >>> s) id as t

That is, `foldl` computes a big *reverse* composition. So, for example, given `[1,2,3]`, we get

    foldr (\ a s -> (`g` a) >>> s) id [1,2,3] t
    = ((`g` 1) >>> (`g` 2) >>> (`g` 3) >>> id) t

where the "pipeline" feeds its argument in from the left, so we get

    ((`g` 1) >>> (`g` 2) >>> (`g` 3) >>> id) t
    = ((`g` 2) >>> (`g` 3) >>> id) (g t 1)
    = ((`g` 3) >>> id) (g (g t 1) 2)
    = id (g (g (g t 1) 2) 3)
    = g (g (g t 1) 2) 3

and if you take `g = flip (:)` and `t = []` you get

    flip (:) (flip (:) (flip (:) [] 1) 2) 3
    = flip (:) (flip (:) (1 : []) 2) 3
    = flip (:) (2 : 1 : []) 3
    = 3 : 2 : 1 : []
    = [3, 2, 1]

That is,

    reverse as = foldr (\ a s -> (a :) >>> s) id as []

by instantiating the *general* transformation of `foldl` to `foldr`.

**For mathochists only.** Do `cabal install newtype` and import `Data.Monoid`, `Data.Foldable` and `Control.Newtype`. Add the tragically missing instance:

    instance Newtype (Dual o) o where
      pack = Dual
      unpack = getDual

Observe that, on the one hand, we can implement `foldMap` by `foldr`

    foldMap :: Monoid x => (a -> x) -> [a] -> x
    foldMap f = foldr (mappend . f) mempty

but also vice versa

    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr f = flip (ala' Endo foldMap f)

so that `foldr` accumulates in the monoid of composing endofunctions, but now to get `foldl`, we tell `foldMap` to work in the `Dual` monoid.

    foldl :: (b -> a -> b) -> b -> [a] -> b
    foldl g = flip (ala' Endo (ala' Dual foldMap) (flip g))

What is `mappend` for `Dual (Endo b)`? Modulo wrapping, it's exactly the reverse composition, `>>>`.
