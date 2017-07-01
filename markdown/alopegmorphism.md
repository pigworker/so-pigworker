**Statutory Calculus Warning.** The basic answer to this question involves specialising a standard recursion scheme. But I got a bit carried away pulling at the thread of it. Things take a more abstract turn as I seek to apply the same method to structures other than lists. I end up reaching for Isaac Newton and Ralph Fox, and in the process devise the *alopegmorphism*, which may be something new.

**But anyway,** something of the sort ought to exist. It looks like a special case of the *anamorphism* or "unfold". Let's start with what's called `unfoldr` in the library.

    unfoldr :: (seed -> Maybe (value, seed)) -> seed -> [value]

It shows how to grow a list of values from a seed, repeatedly using a function called a *coalgebra*. At each step, the coalgebra says whether to stop with `[]` or to carry on by consing a value onto a list grown from a new seed.

    unfoldr coalg s = case coalg s of
      Nothing       -> []
      Just (v, s')  -> v : unfoldr coalg s'

Here, the *seed* type can be whatever you like &mdash; whatever local state is appropriate to the unfolding process. One entirely sensible notion of seed is simply "the list so far", perhaps in reverse order, so that the most recently added elements are nearest.

    growList :: ([value] -> Maybe value) -> [value]
    growList g = unfoldr coalg B0 where
      coalg vz = case g vz of   -- I say "vz", not "vs" to remember it's reversed
        Nothing  -> Nothing
        Just v   -> Just (v, v : vz)

At each step, our `g` operation looks at the context of values we already have and decides whether to add another: if so, the new value becomes both the head of the list and the most recent value in the new context.

So, this `growList` hands you at each step your list of previous results, ready for `zipWith (*)`. The reversal is rather handy for the convolution, so perhaps we're looking at something like

    ps = growList $ \ pz -> Just (sum (zipWith (*) sigmas pz) `div` (length pz + 1))
    sigmas = [sigma j | j <- [1..]]

perhaps?

**A recursion scheme?** For lists, we have a special case of the anamorphism, where the seed is the context of what we've built so far, and once we've said how to build a bit more, we know how to grow the context by the same token. It's not hard to see how that works for lists. But how does it work for anamorphisms in general? **Here's where things get hairy.**

We build up possibly infinite values whose node shape is given by some functor `f` (whose parameter turns out to be "substructures" when we "tie the knot").

    newtype Nu f = In (f (Nu f))

In an anamorphism, the coalgebra uses the seed to choose a shape for the outermost node, populated with seeds for the substructures. (Co)recursively, we map the anamorphism across, growing those seeds into substructures.

    ana :: Functor f => (seed -> f seed) -> seed -> Nu f
    ana coalg s = In (fmap (ana coalg) (coalg s))

Let's reconstruct `unfoldr` from `ana`. We can build lots of ordinary recursive structures from `Nu` and a few simple parts: the *polynomial Functor kit*.

    newtype  K1 a      x  = K1 a                  -- constants (labels)
    newtype  I         x  = I x                   -- substructure places
    data     (f :+: g) x  = L1 (f x) | R1 (g x)   -- choice (like Either)
    data     (f :*: g) x  = f x :*: g x           -- pairing (like (,))

with `Functor` instances

    instance Functor (K1 a) where fmap f (K1 a) = K1 a
    instance Functor I      where fmap f (I s) = I (f s)
    instance (Functor f, Functor g) => Functor (f :+: g) where
      fmap h (L1 fs) = L1 (fmap h fs)
      fmap h (R1 gs) = R1 (fmap h gs)
    instance (Functor f, Functor g) => Functor (f :*: g) where
      fmap h (fx :*: gx) = fmap h fx :*: fmap h gx

For lists of `value`, the node shape functor is

    type ListF value = K1 () :+: (K1 value :*: I)

meaning "either a boring label (for nil) or the (cons) pair of a `value` label and a sublist". The type of a `ListF value` coalgebra becomes

    seed -> (K1 () :+: (K1 value :*: I)) seed

which is isomorphic (by "evaluating" the polynomial `ListF value` at `seed`) to

    seed -> Either () (value, seed)

which is but a hair's breadth from the

    seed -> Maybe (value, seed)

that `unfoldr` expects. You can recover an ordinary list like so

    list :: Nu (ListF a) -> [a]
    list (In (L1 _))                = []
    list (In (R1 (K1 a :*: I as)))  = a : list as

Now, how do we grow some general `Nu f`? A good start is to choose the *shape* for the outermost node. A value of type `f ()` gives just the shape of a node, with trivial stubs in the substructure positions. Indeed, to grow our trees, we basically need some way to choose the "next" node shape given some idea where we've got to and what we've done so far. We should expect

    grow :: (..where I am in a Nu f under construction.. -> f ()) -> Nu f

Note that for growing lists, our step function returns a `ListF value ()`, which is isomorphic to `Maybe value`.

But how do we express where we are in a `Nu f` so far? We're going to be so-many-nodes-in from the root of the structure, so we should expect a stack of layers. Each layer should tell us (1) its shape, (2) which position we're currently at, and (3) the structures already built to the left of that position, but we expect still to have stubs in the positions at which we have not yet arrived. In other words, it's an example of the *dissection* structure from my 2008 POPL paper about [Clowns and Jokers][1].

The dissection operator turns a functor `f` (seen as a container of elements) into a bifunctor `Diss f` with two different sorts of elements, those on the left (clowns) and those on the right (jokers) of a "cursor position" within an `f` structure. First, let's have the `Bifunctor` class and some instances.

    class Bifunctor b where
      bimap :: (c -> c') -> (j -> j') -> b c j -> b c' j'

    newtype K2 a       c j = K2 a
    data    (f :++: g) c j = L2 (f c j) | R2 (g c j)
    data    (f :**: g) c j = f c j :**: g c j
    newtype Clowns f   c j = Clowns (f c)
    newtype Jokers f   c j = Jokers (f j)

    instance Bifunctor (K2 a) where
      bimap h k (K2 a) = K2 a
    instance (Bifunctor f, Bifunctor g) => Bifunctor (f :++: g) where
      bimap h k (L2 fcj) = L2 (bimap h k fcj)
      bimap h k (R2 gcj) = R2 (bimap h k gcj)
    instance (Bifunctor f, Bifunctor g) => Bifunctor (f :**: g) where
      bimap h k (fcj :**: gcj) = bimap h k fcj :**: bimap h k gcj
    instance Functor f => Bifunctor (Clowns f) where
      bimap h k (Clowns fc) = Clowns (fmap h fc)
    instance Functor f => Bifunctor (Jokers f) where
      bimap h k (Jokers fj) = Jokers (fmap k fj)

Note that `Clowns f` is the bifunctor which amounts to an `f` structure containing only clowns, whilst `Jokers f` has only jokers. If you feel bothered by the repetition of all the `Functor` paraphernalia to get the `Bifunctor` paraphernalis, you're right to be bothered: it gets less laborious if we abstract away the arity and work with functors between *indexed* sets, but that's a whole other story.

Let's define *dissection* as a class which associates a bifunctor with a functor.

    class (Functor f, Bifunctor (Diss f)) => Dissectable f where
      type Diss f :: * -> * -> *
      rightward   ::  Either (f j) (Diss f c j, c) ->
                      Either (j, Diss f c j) (f c)

The type `Diss f c j` represents an `f`-structure with a "hole" or "cursor position" at one element position, and in the positions to the left of the hole we have "clowns" in `c`, and to the right we have "jokers" in `j`. (The terminology is lifted from the *Stealer's Wheel* song "Stuck in the Middle with You".)

The key operation in the class is the isomorphism `rightward` which tells us how to move one place to the right, starting from either

  * left of a whole structure full of jokers, or
  * a hole in the structure, together with a clown to put in the hole

and arriving at either

  * a hole in the structure, together with the joker which came out of it, or
  * right of a whole structure full of clowns.

Isaac Newton was fond of dissections, but he called them *divided differences* and defined them on real-valued functions to get the slope between two points on a curve, thus

    divDiff f c j  =  (f c - f j) / (c - j)

and he used them to make best polynomial approximations to any old functions, and the like. Multiply up and multiply out

    divDiff f c j * c - j * divDiff f c j  =  f c - f j

then get rid of the subtraction by adding to both sides

    f j + divDiff f c j * c  =  f c + j * divDiff f c j

and you've got the `rightward` isomorphism.

We might build a bit more intuition for these things if we look at the instances, and then we can get back to our original problem.

A boring old constant has zero as its divided difference.

    instance Dissectable (K1 a) where
      type Diss (K1 a) = K2 Void
      rightward (Left (K1 a)) = (Right (K1 a))
      rightward (Right (K2 v, _)) = absurd v

If we start to the left and go to the right, we jump over the whole structure, because there are no element positions. If we start in an element position, someone is lying!

The identity functor has just *one* position.

    instance Dissectable I where
      type Diss I = K2 ()
      rightward (Left (I j))       = Left (j, K2 ())
      rightward (Right (K2 (), c)) = Right (I c)

If we start to the left, we arrive in the position and out pops the joker; push in a clown and we finish on the right.

For sums, the structure is inherited: we just have to get the detagging and retagging correct.

    instance (Dissectable f, Dissectable g) => Dissectable (f :+: g) where
      type Diss (f :+: g) = Diss f :++: Diss g
      rightward x = case x of
          Left (L1 fj)      -> ll (rightward (Left fj))
          Right (L2 df, c)  -> ll (rightward (Right (df, c)))
          Left (R1 gj)      -> rr (rightward (Left gj))
          Right (R2 dg, c)  -> rr (rightward (Right (dg, c)))
        where
          ll (Left (j, df)) = Left (j, L2 df)
          ll (Right fc)     = Right (L1 fc)
          rr (Left (j, dg)) = Left (j, R2 dg)
          rr (Right gc)     = Right (R1 gc)

For products, we must be somewhere in a pair of structures: either we're on the left between clowns and jokers with the right structure all jokers, or the left structure is all clowns and we're on the right between clowns and jokers.

    instance (Dissectable f, Dissectable g) => Dissectable (f :*: g) where
      type Diss (f :*: g) = (Diss f :**: Jokers g) :++: (Clowns f :**: Diss g)
      rightward x = case x of
          Left (fj :*: gj) -> ll (rightward (Left fj)) gj
          Right (L2 (df :**: Jokers gj), c) -> ll (rightward (Right (df, c))) gj
          Right (R2 (Clowns fc :**: dg), c) -> rr fc (rightward (Right (dg, c)))
        where
          ll (Left (j, df)) gj = Left (j, L2 (df :**: Jokers gj))
          ll (Right fc)     gj = rr fc (rightward (Left gj))  -- (!)
          rr fc (Left (j, dg)) = Left (j, R2 (Clowns fc :**: dg))
          rr fc (Right gc)     = Right (fc :*: gc)

The `rightward` logic ensures that we work our way through the left structure, then once we're done with it, we start work on the right. The line marked `(!)` is the key moment in the middle, where we emerge from the right of the left structure and then enter the left of the right structure.

Huet's notion of "left" and "right" cursor movements in data structures arise from dissectability (if you complete the `rightward` isomorphism with its `leftward` counterpart). The *derivative* of `f` is just the limit when the difference between clowns and jokers tend to zero, or for us, what you get when you have the same sort of stuff either side of the cursor.

Moreover, if you take clowns to be zero, you get

    rightward :: Either (f x) (Diss f Void x, Void) -> Either (x, Diss f Void x) (f Void)

but we can remove the impossible input case to get

    type Quotient f x = Diss f Void x
    leftmost :: f x -> Either (x, Quotient f x) (f Void)
    leftmost = rightward . Left

which tells us that every `f` structure either has a leftmost element or none at all, a result we learn in school as the "Remainder Theorem". The multivariate version of the `Quotient` operator is the "derivative" which Brzozowski applied to regular expressions.

But *our* special case is Fox's derivative (about which I learned from [Dan Piponi][2]):

    type Fox f x = Diss f x ()

That's the type of `f`-structures with stubs to the right of a cursor. *Now* we can give the type of our general `grow` operator.

    grow :: Dissectable f => ([Fox f (Nu f)] -> f ()) -> Nu f

Our "context" is a stack of layers, each of which has fully grown data to the left and stubs to the right. We can implement `grow` directly as follows:

    grow g = go [] where
      go stk = In (walk (rightward (Left (g stk)))) where
        walk (Left ((), df)) = walk (rightward (Right (df, go (df : stk))))
        walk (Right fm)      = fm

As we arrive at each position, the joker we extract is just a stub, but its context tells us how to extend the stack in order to grow a substructure of the tree, which gives us the clown that we need to move right. Once we've filled in all the stubs with trees, we're done!

But here's the twist: `grow` is not so easy to express as an anamorphism. It's easy to give the "seed" for the leftmost child of each node, because we have only stubs to the right of us. But to give the next seed to the right, we need more than the leftmost seed &mdash; we need the tree that grows from it! The anamorphism pattern requires us to give all the seeds for substructures before growing any of them. Our `growList` is an anamorphism only because list nodes have *at most one* child.

So it's something new, after all, growing from nothing, but allowing later growth at a given layer to depend on earlier trees, with the Fox derivative capturing the idea of "stubs where we have yet to work". Perhaps we should call it an *alopegmorphism*, from the Greek &alpha;&lambda;&omega;&pi;&eta;&xi; for "fox".

  [1]: http://strictlypositive.org/CJ.pdf
  [2]: http://blog.sigfpe.com/2007/01/foxs-ubiquitous-free-derivative.html
