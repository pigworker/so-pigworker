> And Noah said unto the animals "Go forth and multiply!", but the snakes said
> "We cannot multiply, for we are adders.", so Noah took wood from the Ark and,
> shaping it, said "I am building you a table of logs.".

Representable functors are sometimes also called "Naperian" functors (it's Peter Hancock's term: Hank's a denizen of the same part of Edinburgh as John Napier, of logarithmic fame) because when `F x ~= T -> x`, and remembering that, combinatorially, `T -> x` is "`x` to the power `T`", we see that `T` is in some sense `Log F`.

The first thing to note is that `F () ~= T -> () ~= ()`. That tells us *there is only one shape*. Functors which offer us a choice of shape cannot be Naperian, because they don't give a uniform presentation of the positions for data. That means `[]` is not Naperian, because different-length lists have positions represented by different types. However, an infinite `Stream` has positions given by the natural numbers.

Correspondingly, given any two `F` structures, their shapes are bound to match, so they have a sensible `zip`, giving us the basis for an `Applicative F` instance.

Indeed, we have

              a  -> p x
    =====================
      (Log p, a) ->   x

making `p` a right adjoint, so `p` preserves all limits, hence unit and products in particular, making it a monoidal functor, not just a *lax* monoidal functor. That is, the alternative presentation of `Applicative` has operations which are isomorphisms.

    unit  :: ()         ~= p ()
    mult  :: (p x, p y) ~= p (x, y)

Let's have a type class for the things. I cook it a bit differently from the `Representable` class.

    class Applicative p => Naperian p where
      type Log p
      logTable  :: p (Log p)
      project   :: p x -> Log p -> x
      tabulate  :: (Log p -> x) -> p x
      tabulate f = fmap f logTable
      -- LAW1: project logTable = id
      -- LAW2: project px <$> logTable = px

We have a type `Log f`, representing at least some of the positions inside an `f`; we have a `logTable`, storing in each position the representative of that position, acting like a 'map of an `f`' with placenames in each place; we have a `project` function extracting the data stored at a given position.

The first law tells us that the `logTable` is accurate for all the positions which are represented. The second law tells us that we have represented *all* the positions. We may deduce that

    tabulate (project px)
      = {definition}
    fmap (project px) logTable
      = {LAW2}
    px

and that

    project (tabulate f)
      = {definition}
    project (fmap f logTable)
      = {free theorem for project}
    f . project logTable
      = {LAW1}
    f . id
      = {composition absorbs identity}
    f

We could imagine a generic instance for `Applicative`

    instance Naperian p => Applicative p where
      pure x    = fmap (pure x)                    logTable
      pf <$> px = fmap (project pf <*> project ps) logTable

which is as much as to say that `p` inherits its own K and S combinators from the usual K and S for functions.

Of course, we have

    instance Naperian ((->) r) where
      type Log ((->) r) = r  -- log_x (x^r) = r
      logTable = id
      project = ($)

Now, all the limit-like constructions preserve Naperianity. `Log` maps limity things to colimity things: it *calculates* left adjoints.

We have the terminal object and products.

    data K1       x = K1
    instance Applicative K1 where
      pure x    = K1
      K1 <*> K1 = K1
    instance Functor K1 where fmap = (<*>) . pure

    instance Naperian K1 where
      type Log K1 = Void -- "log of 1 is 0"
      logTable = K1
      project K1 nonsense = absurd nonsense
  
    data (p * q)  x = p x :*: q x
    instance (Applicative p, Applicative q) => Applicative (p * q) where
      pure x = pure x :*: pure x
      (pf :*: qf) <*> (ps :*: qs) = (pf <*> ps) :*: (qf <*> qs)
    instance (Functor p, Functor q) => Functor (p * q) where
      fmap f (px :*: qx) = fmap f px :*: fmap f qx

    instance (Naperian p, Naperian q) => Naperian (p * q) where
      type Log (p * q) = Either (Log p) (Log q)  -- log (p * q) = log p + log q
      logTable = fmap Left logTable :*: fmap Right logTable
      project (px :*: qx) (Left i)  = project px i
      project (px :*: qx) (Right i) = project qx i

We have identity and composition.

    data I        x = I x
    instance Applicative I where
      pure x = I x
      I f <*> I s = I (f s)
    instance Functor I where fmap = (<*>) . pure

    instance Naperian I where
      type Log I = ()    -- log_x x = 1
      logTable = I ()
      project (I x) () = x

    data (p << q) x = C (p (q x))
    instance (Applicative p, Applicative q) => Applicative (p << q) where
      pure x = C (pure (pure x))
      C pqf <*> C pqs = C (pure (<*>) <*> pqf <*> pqs)
    instance (Functor p, Functor q) => Functor (p << q) where
      fmap f (C pqx) = C (fmap (fmap f) pqx)

    instance (Naperian p, Naperian q) => Naperian (p << q) where
      type Log (p << q) = (Log p, Log q)  -- log (q ^ log p) = log p * log q
      logTable = C (fmap (\ i -> fmap (i ,) logTable) logTable)
      project (C pqx) (i, j) = project (project pqx i) j

Naperian functors are closed under *greatest* fixpoints, with their logarithms being the corresponding *least* fixpoints. E.g., for streams, we have

    log_x (Stream x)
      =
    log_x (nu y. x * y)
      =
    mu log_xy. log_x (x * y)
      =
    mu log_xy. log_x x + log_x y
      =
    mu log_xy. 1 + log_xy
      =
    Nat

It's a bit fiddly to render that in Haskell without introducing Naperian *bifunctors* (which have two sets of positions for two sorts of things), or (better) Naperian functors on indexed types (which have indexed positions for indexed things). What's easy, though, and hopefully gives the idea, is the cofree comonad.

    data{-codata-} CoFree p x = x :- p (CoFree p x)
      -- i.e., (I * (p << CoFree p)) x
    instance Applicative p => Applicative (CoFree p) where
      pure x = x :- pure (pure x)
      (f :- pcf) <*> (s :- pcs) = f s :- (pure (<*>) <*> pcf <*> pcs)
    instance Functor p => Functor (CoFree p) where
      fmap f (x :- pcx) = f x :- fmap (fmap f) pcx

    instance Naperian p => Naperian (CoFree p) where
      type Log (CoFree p) = [Log p]  -- meaning finite lists only
      logTable = [] :- fmap (\ i -> fmap (i :) logTable) logTable
      project (x :- pcx) []       = x
      project (x :- pcx) (i : is) = project (project pcx i) is

We may take `Stream = CoFree I`, giving

    Log Stream = [Log I] = [()] ~= Nat

Now, the derivative `D p` of a functor gives its type of one-hole context, telling us i) the shape of a `p`, ii) the position of the hole, iii) the data that are not in the hole. If `p` is Naperian, there is no choice of shape, so putting trivial data in the non-hole positions, we find that we just get the position of the hole.

    D p () ~= Log p

More on that connection can be found in [this answer of mine](https://stackoverflow.com/a/45254970/828361) about tries.

Anyhow, Naperian is indeed a funny local Scottish name for Representable, which are the things for which you can build a table of logs: they are the constructions characterized entirely by projection, offering no choice of 'shape'.
