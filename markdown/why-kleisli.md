In a sense, you're right. As every monad `m` is a functor, we can use `fmap f` with a function `f :: a -> b` to turn an `m a` into an `m b`, but there's a catch. What's `b`?

I like to think of such an `m` as meaning "plan-to-get", where "plans" involve some sort of additional interaction beyond pure computation. If you have a "plan-to-get `Int`" and you want a "plan-to-get `String`", you can use `fmap` with a function in `Int -> String`, but the type of that function tells you that getting the `String` from the `Int` involves no further interaction.

That isn't always so: perhaps the `Int` is a student registration number and the `String` is their name, so the plan to convert from one to the other needs an external lookup in some table. Then I don't have a pure function from `Int` to `String`, but rather a pure function from `Int` to "plan-to-get `String`". If I `fmap` that across my "plan-to-get `Int`", that's fine, but I end up with "plan-to-get (plan-to-get `String`)" and I need to `join` the outer and inner plans.

The general situation is that we have enough information to compute the plan to get more. That's what `a -> m b` models. In particular, we have `return :: a -> m a`, which turns the information we have into the plan that gives us exactly that information by taking no further action, and we have `(>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)` which composes two such things. We also have that `(>=>)` is associative and absorbs `return` on left and right, much the way `;` is associative and absorbs `skip` in classic imperative programming.

It's more convenient to build larger plans from smaller ones using this compositional approach, keeping the number of "plan-to-get" layers a consistent *one*. Otherwise, you need to build up an *n*-layer plan with `fmap`, then do the right number of `join`s on the outside (which will be a brittle property of the plan).

Now, as Haskell is a language with a concept of "free variable" and "scope", the `a` in

    (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

representing the "overall input information" can just be taken to come from the scope of things we already have, leaving

    (>>=) ::       m b  -> (b -> m c) ->       m c

and we get back "bind", which is the tool that presents the compositional structure in the most programmer-friendly form, resembling a local definition.

To sum up, you can work with `a -> b`, but often you need `b` to be "plan-to-get something", and that's the helpful thing to choose if you want to build plans compositionally.
