Consider this representation for lambda terms parametrized by their free variables. (See papers by Bellegarde and Hook 1994, Bird and Paterson 1999, Altenkirch and Reus 1999.)

    data Tm a  = Var a
               | Tm a :$ Tm a
               | Lam (Tm (Maybe a))

You can certainly make this a `Functor`, capturing the notion of renaming, and a `Monad` capturing the notion of substitution.

    instance Functor Tm where
      fmap rho (Var a)   = Var (rho a)
      fmap rho (f :$ s)  = fmap rho f :$ fmap rho s
      fmap rho (Lam t)   = Lam (fmap (fmap rho) t)

    instance Monad Tm where
      return = Var
      Var a     >>= sig  = sig a
      (f :$ s)  >>= sig  = (f >>= sig) :$ (s >>= sig)
      Lam t     >>= sig  = Lam (t >>= maybe (Var Nothing) (fmap Just . sig))

Now consider the *closed* terms: these are the inhabitants of `Tm Void`. You should be able to embed the closed terms into terms with arbitrary free variables. How?

    fmap absurd :: Tm Void -> Tm a

The catch, of course, is that this function will traverse the term doing precisely nothing. But it's a touch more honest than `unsafeCoerce`. And that's why `vacuous` was added to `Data.Void`...

Or write an evaluator. Here are values with free variables in `b`.

    data Val b
      =  b :$$ [Val b]                              -- a stuck application
      |  forall a. LV (a -> Val b) (Tm (Maybe a))   -- we have an incomplete environment

I've just represented lambdas as closures. The evaluator is parametrized by an environment mapping free variables in `a` to values over `b`.

    eval :: (a -> Val b) -> Tm a -> Val b
    eval g (Var a)   = g a
    eval g (f :$ s)  = eval g f $$ eval g s where
      (b :$$ vs)  $$ v  = b :$$ (vs ++ [v])         -- stuck application gets longer
      LV g t      $$ v  = eval (maybe v g) t        -- an applied lambda gets unstuck
    eval g (Lam t)   = LV g t

You guessed it. To evaluate a closed term at any target

    eval absurd :: Tm Void -> Val b

More generally, `Void` is seldom used on its own, but is handy when you want to instantiate a type parameter in a way which indicates some sort of impossibility (e.g., here, using a free variable in a closed term). Often these parametrized types come with higher-order functions lifting operations on the parameters to operations on the whole type (e.g., here, `fmap`, `>>=`, `eval`). So you pass `absurd` as the general-purpose operation on `Void`.

For another example, imagine using `Either e v` to capture computations which hopefully give you a `v` but might raise an exception of type `e`. You might use this approach to document risk of bad behaviour uniformly. For perfectly well behaved computations in this setting, take `e` to be `Void`, then use

    either absurd id :: Either Void v -> v

to run safely or

    either absurd Right :: Either Void v -> Either e v

to embed safe components in an unsafe world.

Oh, and one last hurrah, handling a "can't happen". It shows up in the generic zipper construction, everywhere that the cursor can't be.

    class Differentiable f where
      type D f :: * -> *              -- an f with a hole
      plug :: (D f x, x) -> f x       -- plugging a child in the hole

    newtype K a     x  = K a          -- no children, just a label
    newtype I       x  = I x          -- one child
    data (f :+: g)  x  = L (f x)      -- choice
                       | R (g x)
    data (f :*: g)  x  = f x :&: g x  -- pairing

    instance Differentiable (K a) where
      type D (K a) = K Void           -- no children, so no way to make a hole
      plug (K v, x) = absurd v        -- can't reinvent the label, so deny the hole!

I decided not to delete the rest, even though it's not exactly relevant.

    instance Differentiable I where
      type D I = K ()
      plug (K (), x) = I x

    instance (Differentiable f, Differentiable g) => Differentiable (f :+: g) where
      type D (f :+: g) = D f :+: D g
      plug (L df, x) = L (plug (df, x))
      plug (R dg, x) = R (plug (dg, x))

    instance (Differentiable f, Differentiable g) => Differentiable (f :*: g) where
      type D (f :*: g) = (D f :*: g) :+: (f :*: D g)
      plug (L (df :&: g), x) = plug (df, x) :&: g
      plug (R (f :&: dg), x) = f :&: plug (dg, x)

Actually, maybe it is relevant. If you're feeling adventurous, this [unfinished article][1] shows how to use `Void` to compress the representation of terms with free variables

    data Term f x = Var x | Con (f (Term f x))   -- the Free monad, yet again

in any syntax generated freely from a `Differentiable` and `Traversable` functor `f`. We use `Term f Void` to represent regions with no free variables, and `[D f (Term f Void)]` to represent *tubes* tunnelling through regions with no free variables either to an isolated free variable, or to a junction in the paths to two or more free variables. Must finish that article sometime.

For a type with no values (or at least, none worth speaking of in polite company), `Void` is remarkably useful. And `absurd` is how you use it.

  [1]: http://www.strictlypositive.org/Holes.pdf
