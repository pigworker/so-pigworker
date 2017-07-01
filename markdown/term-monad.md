For reference, here are the terms...

    data Term a = 
          Var a
        | Lambda a (Term a)
        | Apply (Term a) (Term a)

(I note that the representation of variables is abstracted, which is often a good plan.)

...and here is the proposed function.

    fmap' :: (Term a → Term a) → Term a → Term a 
    fmap' f (Var v) = f (Var v)
    fmap' f (Lambda v t) = Lambda v (fmap' f t)
    fmap' f (Apply t1 t2) = Apply (fmap' f t1) (fmap' f t2)

What bothers me about this definition is that `f` is only ever applied to terms of form `(Var v)`, so you might as well implement *substitution*.

    subst :: (a → Term a) → Term a → Term a 
    subst f (Var v) = f v
    subst f (Lambda v t) = Lambda v (subst f t)
    subst f (Apply t1 t2) = Apply (subst f t1) (subst f t2)

If you took slightly more care to distinguish bound from free variables, you'd be able to make `Term` a `Monad` with substitution implementing `(>>=)`. In general, terms can have a `Functor` structure for renaming and a `Monad` structure for substitution. There's a lovely paper by [Bird and Paterson][1] about that, but I digress.

Meanwhile, if you *do* want to act other than at variables, one general approach is to use general purpose traversal toolkits like uniplate, as augustss suggests. Another possibility, perhaps slightly more disciplined, is to work with the &lsquo;fold&rsquo; for your type.

    tmFold :: (x -> y) -> (x -> y -> y) -> (y -> y -> y) -> Term x -> y
    tmFold v l a (Var x)       = v x
    tmFold v l a (Lambda x t)  = l x (tmFold v l a t)
    tmFold v l a (Apply t t')  = a (tmFold v l a t) (tmFold v l a t')

Here, `v`, `l` and `a` define an alternative *algebra* for your `Term`-forming operations, only acting on `y`, rather than `Term x`, explaining how to handle variables, lambdas and applications. You might choose `y` to be `m (Term x)` for some suitable monad `m` (e.g., threading an environment for the variables), rather than just `Term x` itself. Each subterm is processed to give a `y`, then the appropriate function is chosen to produce the `y` for the whole term. The fold captures the standard recursion pattern.

Ordinary first-order datatypes (and some well-behaved higher-order datatypes) can all be equipped with fold-operators. At a cost to readability, you can even write the fold operator once and for all.

    data Fix f = In (f (Fix f))

    fixFold :: Functor f => (f y -> y) -> Fix f -> y
    fixFold g (In xf) = g (fmap (fixFold g) xf)

    data TermF a t
      = VarF a
      | LambdaF a t
      | ApplyF t t

    type Term a = Fix (TermF a)

Unlike your recursive `Term a`, this `TermF a t` explains how to make *one* layer of a term, with `t` elements in the subterm places. We get back the recursive `Term` structure by using the recursive `Fix` type. We lose a little cosmetically, in that each layer has an extra `In` wrapping it. We can define

    var x      = In (VarF x)
    lambda x t = In (LambdaF x t)
    apply t t' = In (Apply x t t')

but we can't use those definitions in pattern matching. The payoff, though, is that we can use the generic `fixFold` at no extra cost. To compute a `y` from a term, we need only give a function of type

    TermF a y -> y

which (just like `v`, `l`, and `a` above) explains how to handle any term whose subterms have already been processed to values of type `y`. By being explicit in types about what one layer consists of, we can tap into the general pattern of working layer by layer.

  [1]: http://www.cs.ox.ac.uk/people/richard.bird/online/BirdPaterson99DeBruijn.pdf
