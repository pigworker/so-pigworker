There's a classic, somewhat alarming example (involving interaction with functional dependencies) in [this paper from HQ][1]:

    class Mul a b c | a b -> c where
      mul :: a -> b -> c
    instance Mul a b c => Mul a [b] [c] where
      mul a = map (mul a)

    f b x y = if b then mul x [y] else y

We need `mul x [y]` to have the same type as `y`, so, taking `x :: x` and `y :: y`, we need

    instance Mul x [y] y

which, according to the given instance, we can have. Of course, we must take `y ~ [z]` for some `z` such that

    instance Mul x y z

i.e.

    instance Mul x [z] z

and we're in a loop.

It's disturbing, because that `Mul` instance looks like its recursion is structurally *reducing*, unlike the clearly pathological instance in Petr's answer. Yet it makes GHC loop (with the boredom threshold kicking in to avoid hanging).

The trouble, as I'm sure I've mentioned somewhere somewhen, is that the identification `y ~ [z]` is made in spite of the fact that `z` depends functionally on `y`. If we used a functional notation for the functional dependency, we might see that the constraint says `y ~ Mul x [y]` and reject the substitution as in violation of the occurrence check.

Intrigued, I thought I'd give this a whirl,

    class Mul' a b where
      type MulT a b
      mul' :: a -> b -> MulT a b

    instance Mul' a b => Mul' a [b] where
      type MulT a [b] = [MulT a b]
      mul' a = map (mul' a)

    g b x y = if b then mul' x [y] else y

With `UndecidableInstances` enabled, it takes quite a while for the loop to time out. With `UndecidableInstances` disabled, the instance is still accepted and the typechecker still loops, but the cutoff happens much more quickly.

So... funny old world.

  [1]: http://research.microsoft.com/en-us/um/people/simonpj/papers/fd-chr/
