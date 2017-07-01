Like the childcatcher in Chitty-Chitty-Bang-Bang luring kids into captivity with sweets and toys, recruiters to undergraduate Physics like to fool about with soap bubbles and boomerangs, but when the door clangs shut, it's "Right, children, time to learn about partial differentiation!". Me too. Don't say I didn't warn you.

Here's another warning: the following code needs `{-# LANGUAGE KitchenSink #-}`, or rather

    {-# LANGUAGE TypeFamilies, FlexibleContexts, TupleSections, GADTs, DataKinds,
        TypeOperators, FlexibleInstances, RankNTypes, ScopedTypeVariables,
        StandaloneDeriving, UndecidableInstances #-}

in no particular order.


<h3>Differentiable functors give comonadic zippers</h3>

What is a differentiable functor, anyway?

    class (Functor f, Functor (DF f)) => Diff1 f where
      type DF f :: * -> *
      upF      ::  ZF f x  ->  f x
      downF    ::  f x     ->  f (ZF f x)
      aroundF  ::  ZF f x  ->  ZF f (ZF f x)

    data ZF f x = (:<-:) {cxF :: DF f x, elF :: x}

It's a functor which has a derivative, which is also a functor. The derivative represents a one-hole context for an *element*. The zipper type `ZF f x` represents the pair of a one-hole context and the element in the hole.

The operations for `Diff1` describe the kinds of navigation we can do on zippers (without any notion of "leftward" and "rightward", for which see my [Clowns and Jokers][1] paper). We can go "upward", reassembling the structure by plugging the element in its hole. We can go "downward", finding every way to visit an element in a give structure: we decorate every element with its context. We can go "around",
taking an existing zipper and decorating each element with its context, so we find all the ways to refocus (and how to keep our current focus).

Now, the type of `aroundF` might remind some of you of

    class Functor c => Comonad c where
      extract    :: c x -> x
      duplicate  :: c x -> c (c x)

and you're right to be reminded! We have, with a hop and a skip,

    instance Diff1 f => Functor (ZF f) where
      fmap f (df :<-: x) = fmap f df :<-: f x

    instance Diff1 f => Comonad (ZF f) where
      extract    = elF
      duplicate  = aroundF

and we insist that

    extract . duplicate == id
    fmap extract . duplicate == id
    duplicate . duplicate == fmap duplicate . duplicate

We also need that

    fmap extract (downF xs) == xs              -- downF decorates the element in position
    fmap upF (downF xs) = fmap (const xs) xs   -- downF gives the correct context

<h3>Polynomial functors are differentiable</h3>

**Constant** functors are differentiable.

    data KF a x = KF a
    instance Functor (KF a) where
      fmap f (KF a) = KF a

    instance Diff1 (KF a) where
      type DF (KF a) = KF Void
      upF (KF w :<-: _) = absurd w
      downF (KF a) = KF a
      aroundF (KF w :<-: _) = absurd w

There's nowhere to put an element, so it's impossible to form a context. There's nowhere to go `upF` or `downF` from, and we easily find all none of the ways to go `downF`.

The **identity** functor is differentiable.

    data IF x = IF x
    instance Functor IF where
      fmap f (IF x) = IF (f x)

    instance Diff1 IF where
      type DF IF = KF ()
      upF (KF () :<-: x) = IF x
      downF (IF x) = IF (KF () :<-: x)
      aroundF z@(KF () :<-: x) = KF () :<-: z

There's one element in a trivial context, `downF` finds it, `upF` repacks it, and `aroundF` can only stay put.

**Sum** preserves differentiability.

    data (f :+: g) x = LF (f x) | RF (g x)
    instance (Functor f, Functor g) => Functor (f :+: g) where
      fmap h (LF f) = LF (fmap h f)
      fmap h (RF g) = RF (fmap h g)

    instance (Diff1 f, Diff1 g) => Diff1 (f :+: g) where
      type DF (f :+: g) = DF f :+: DF g
      upF (LF f' :<-: x) = LF (upF (f' :<-: x))
      upF (RF g' :<-: x) = RF (upF (g' :<-: x))

The other bits and pieces are a bit more of a handful. To go `downF`, we must go `downF` inside the tagged component, then fix up the resulting zippers to show the tag in the context.

      downF (LF f) = LF (fmap (\ (f' :<-: x) -> LF f' :<-: x) (downF f))
      downF (RF g) = RF (fmap (\ (g' :<-: x) -> RF g' :<-: x) (downF g))

To go `aroundF`, we strip the tag, figure out how to go around the untagged thing, then restore the tag in all the resulting zippers. The element in focus, `x`, is replaced by its entire zipper, `z`.

      aroundF z@(LF f' :<-: (x :: x)) =
        LF (fmap (\ (f' :<-: x) -> LF f' :<-: x) . cxF $ aroundF (f' :<-: x :: ZF f x))
        :<-: z
      aroundF z@(RF g' :<-: (x :: x)) =
        RF (fmap (\ (g' :<-: x) -> RF g' :<-: x) . cxF $ aroundF (g' :<-: x :: ZF g x))
        :<-: z

Note that I had to use `ScopedTypeVariables` to disambiguate the recursive calls to `aroundF`. As a type function, `DF` is not injective, so the fact that `f' :: D f x` is not enough to force `f' :<-: x :: Z f x`.

**Product** preserves differentiability.

    data (f :*: g) x = f x :*: g x
    instance (Functor f, Functor g) => Functor (f :*: g) where
      fmap h (f :*: g) = fmap h f :*: fmap h g

To focus on an element in a pair, you either focus on the left and leave the right alone, or vice versa. Leibniz's famous product rule corresponds to a simple spatial intuition!

    instance (Diff1 f, Diff1 g) => Diff1 (f :*: g) where
      type DF (f :*: g) = (DF f :*: g) :+: (f :*: DF g)
      upF (LF (f' :*: g) :<-: x) = upF (f' :<-: x) :*: g
      upF (RF (f :*: g') :<-: x) = f :*: upF (g' :<-: x)

Now, `downF` works similarly to the way it did for sums, except that we have to fix up the zipper context not only with a tag (to show which way we went) but also with the untouched other component.

      downF (f :*: g)
        =    fmap (\ (f' :<-: x) -> LF (f' :*: g) :<-: x) (downF f)
        :*:  fmap (\ (g' :<-: x) -> RF (f :*: g') :<-: x) (downF g)

But `aroundF` is a massive bag of laughs. Whichever side we are currently visiting, we have two choices:

 1. Move `aroundF` on that side.
 2. Move `upF` out of that side and `downF` into the other side.

Each case requires us to make use of the operations for the substructure, then fix up contexts.

      aroundF z@(LF (f' :*: g) :<-: (x :: x)) =
        LF (fmap (\ (f' :<-: x) -> LF (f' :*: g) :<-: x)
              (cxF $ aroundF (f' :<-: x :: ZF f x))
            :*: fmap (\ (g' :<-: x) -> RF (f :*: g') :<-: x) (downF g))
        :<-: z
        where f = upF (f' :<-: x)
      aroundF z@(RF (f :*: g') :<-: (x :: x)) =
        RF (fmap (\ (f' :<-: x) -> LF (f' :*: g) :<-: x) (downF f) :*:
            fmap (\ (g' :<-: x) -> RF (f :*: g') :<-: x)
              (cxF $ aroundF (g' :<-: x :: ZF g x)))
        :<-: z
        where g = upF (g' :<-: x)

Phew! The polynomials are all differentiable, and thus give us comonads.

Hmm. It's all a bit abstract. So I added `deriving Show` everywhere I could, and threw in

    deriving instance (Show (DF f x), Show x) => Show (ZF f x)

which allowed the following interaction (tidied up by hand)

    > downF (IF 1 :*: IF 2)
    IF (LF (KF () :*: IF 2) :<-: 1) :*: IF (RF (IF 1 :*: KF ()) :<-: 2)

    > fmap aroundF it
    IF  (LF (KF () :*: IF (RF (IF 1 :*: KF ()) :<-: 2)) :<-: (LF (KF () :*: IF 2) :<-: 1))
    :*:
    IF  (RF (IF (LF (KF () :*: IF 2) :<-: 1) :*: KF ()) :<-: (RF (IF 1 :*: KF ()) :<-: 2))

**Exercise** Show that the composition of differentiable functors is differentiable, using the *chain rule*.

Sweet! Can we go home now? Of course not. We haven't differentiated any *recursive* structures yet.

<h3>Making recursive functors from bifunctors</h3>

A `Bifunctor`, as the existing literature on datatype generic programming (see work by Patrik Jansson and Johan Jeuring, or excellent lecture notes by Jeremy Gibbons) explains at length is a type constructor with two parameters, corresponding to two sorts of substructure. We should be able to "map" both.

    class Bifunctor b where
      bimap :: (x -> x') -> (y -> y') -> b x y -> b x' y'

We can use `Bifunctor`s to give the node structure of recursive containers. Each node has *subnodes* and *elements*. These can just be the two sorts of substructure.

    data Mu b y = In (b (Mu b y) y)

See? We "tie the recursive knot" in `b`'s first argument, and keep the parameter `y` in its second. Accordingly, we obtain once for all

    instance Bifunctor b => Functor (Mu b) where
      fmap f (In b) = In (bimap (fmap f) f b)

To use this, we'll need a kit of `Bifunctor` instances.

<h3>The Bifunctor Kit</h3>

**Constants** are bifunctorial.

    newtype K a x y = K a

    instance Bifunctor (K a) where
      bimap f g (K a) = K a

You can tell I wrote this bit first, because the identifiers are shorter, but that's good because the code is longer.

**Variables** are bifunctorial.

We need the bifunctors corresponding to one parameter or the other, so I made a datatype to distinguish them, then defined a suitable GADT.

    data Var = X | Y

    data V :: Var -> * -> * -> * where
      XX :: x -> V X x y
      YY :: y -> V Y x y

That makes `V X x y` a copy of `x` and `V Y x y` a copy of `y`. Accordingly

    instance Bifunctor (V v) where
      bimap f g (XX x) = XX (f x)
      bimap f g (YY y) = YY (g y)

**Sums** and **Products** of bifunctors are bifunctors

    data (:++:) f g x y = L (f x y) | R (g x y) deriving Show

    instance (Bifunctor b, Bifunctor c) => Bifunctor (b :++: c) where
      bimap f g (L b) = L (bimap f g b)
      bimap f g (R b) = R (bimap f g b)

    data (:**:) f g x y = f x y :**: g x y deriving Show

    instance (Bifunctor b, Bifunctor c) => Bifunctor (b :**: c) where
      bimap f g (b :**: c) = bimap f g b :**: bimap f g c

So far, so boilerplate, but now we can define things like

    List = Mu (K () :++: (V Y :**: V X))

    Bin = Mu (V Y :**: (K () :++: (V X :**: V X)))

If you want to use these types for actual data and not go blind in the pointilliste tradition of Georges Seurat, use *pattern synonyms*.

But what of zippers? How shall we show that `Mu b` is differentiable? We shall need to show that `b` is differentiable in *both* variables. Clang! It's time to learn about partial differentiation.

<h3>Partial derivatives of bifunctors</h3>

Because we have two variables, we shall need to be able to talk about them collectively sometimes and individually at other times. We shall need the singleton family:

    data Vary :: Var -> * where
      VX :: Vary X
      VY :: Vary Y

Now we can say what it means for a Bifunctor to have partial derivatives at each variable, and give the corresponding notion of zipper.

    class (Bifunctor b, Bifunctor (D b X), Bifunctor (D b Y)) => Diff2 b where
      type D b (v :: Var) :: * -> * -> *
      up      :: Vary v -> Z b v x y -> b x y
      down    :: b x y -> b (Z b X x y) (Z b Y x y)
      around  :: Vary v -> Z b v x y -> Z b v (Z b X x y) (Z b Y x y)

    data Z b v x y = (:<-) {cxZ :: D b v x y, elZ :: V v x y}

This `D` operation needs to know which variable to target. The corresponding zipper `Z b v` tells us which variable `v` must be in focus. When we "decorate with context", we have to decorate `x`-elements with `X`-contexts and `y`-elements with `Y`-contexts. But otherwise, it's the same story.

We have two remaining tasks: firstly, to show that our bifunctor kit is differentiable; secondly, to show that `Diff2 b` allows us to establish `Diff1 (Mu b)`.

<h3>Differentiating the Bifunctor kit</h3>

I'm afraid this bit is fiddly rather than edifying. Feel free to skip along.

The constants are as before.

    instance Diff2 (K a) where
      type D (K a) v = K Void
      up _ (K q :<- _) = absurd q
      down (K a) = K a
      around _ (K q :<- _) = absurd q

On this occasion, life is too short to develop the theory of the type level Kronecker-delta, so I just treated the variables separately.

    instance Diff2 (V X) where
      type D (V X) X = K ()
      type D (V X) Y = K Void
      up VX (K () :<- XX x)  = XX x
      up VY (K q :<- _)      = absurd q
      down (XX x) = XX (K () :<- XX x)
      around VX z@(K () :<- XX x)  = K () :<- XX z
      around VY (K q :<- _)        = absurd q

    instance Diff2 (V Y) where
      type D (V Y) X = K Void
      type D (V Y) Y = K ()
      up VX (K q :<- _)      = absurd q
      up VY (K () :<- YY y)  = YY y
      down (YY y) = YY (K () :<- YY y)
      around VX (K q :<- _)        = absurd q
      around VY z@(K () :<- YY y)  = K () :<- YY z

For the structural cases, I found it useful to introduce a helper allowing me to treat variables uniformly.

    vV :: Vary v -> Z b v x y -> V v (Z b X x y) (Z b Y x y)
    vV VX z = XX z
    vV VY z = YY z

I then built gadgets to facilitate the kind of "retagging" we need for `down` and `around`. (Of course, I saw which gadgets I needed as I was working.)

    zimap :: (Bifunctor c) => (forall v. Vary v -> D b v x y -> D b' v x y) ->
             c (Z b X x y) (Z b Y x y) -> c (Z b' X x y) (Z b' Y x y)
    zimap f = bimap
      (\ (d :<- XX x) -> f VX d :<- XX x)
      (\ (d :<- YY y) -> f VY d :<- YY y)

    dzimap :: (Bifunctor (D c X), Bifunctor (D c Y)) =>
             (forall v. Vary v -> D b v x y -> D b' v x y) ->
             Vary v -> Z c v (Z b X x y) (Z b Y x y) -> D c v (Z b' X x y) (Z b' Y x y)
    dzimap f VX (d :<- _) = bimap
      (\ (d :<- XX x) -> f VX d :<- XX x)
      (\ (d :<- YY y) -> f VY d :<- YY y)
      d
    dzimap f VY (d :<- _) = bimap
      (\ (d :<- XX x) -> f VX d :<- XX x)
      (\ (d :<- YY y) -> f VY d :<- YY y)
      d

And with that lot ready to go, we can grind out the details. Sums are easy.

    instance (Diff2 b, Diff2 c) => Diff2 (b :++: c) where
      type D (b :++: c) v = D b v :++: D c v
      up v (L b' :<- vv) = L (up v (b' :<- vv))
      down (L b) = L (zimap (const L) (down b))
      down (R c) = R (zimap (const R) (down c))
      around v z@(L b' :<- vv :: Z (b :++: c) v x y)
        = L (dzimap (const L) v ba) :<- vV v z
        where ba = around v (b' :<- vv :: Z b v x y)
      around v z@(R c' :<- vv :: Z (b :++: c) v x y)
        = R (dzimap (const R) v ca) :<- vV v z
        where ca = around v (c' :<- vv :: Z c v x y)

Products are hard work, which is why I'm a mathematician rather than an engineer.

    instance (Diff2 b, Diff2 c) => Diff2 (b :**: c) where
      type D (b :**: c) v = (D b v :**: c) :++: (b :**: D c v)
      up v (L (b' :**: c) :<- vv) = up v (b' :<- vv) :**: c
      up v (R (b :**: c') :<- vv) = b :**: up v (c' :<- vv)
      down (b :**: c) =
        zimap (const (L . (:**: c))) (down b) :**: zimap (const (R . (b :**:))) (down c)
      around v z@(L (b' :**: c) :<- vv :: Z (b :**: c) v x y)
        = L (dzimap (const (L . (:**: c))) v ba :**:
            zimap (const (R . (b :**:))) (down c))
          :<- vV v z where
          b = up v (b' :<- vv :: Z b v x y)
          ba = around v (b' :<- vv :: Z b v x y)
      around v z@(R (b :**: c') :<- vv :: Z (b :**: c) v x y)
        = R (zimap (const (L . (:**: c))) (down b):**:
            dzimap (const (R . (b :**:))) v ca)
          :<- vV v z where
          c = up v (c' :<- vv :: Z c v x y)
          ca = around v (c' :<- vv :: Z c v x y)

Conceptually, it's just as before, but with more bureaucracy. I built these using pre-type-hole technology, using `undefined` as a stub in places I wasn't ready to work, and introducing a deliberate type error in the one place (at any given time) where I wanted a useful hint from the typechecker. You too can have the typechecking as videogame experience, even in Haskell.

<h3>Subnode zippers for recursive containers</h3>

The partial derivative of `b` with respect to `X` tells us how to find a subnode one step inside a node, so we get the conventional notion of zipper.

    data MuZpr b y = MuZpr
      {  aboveMu  :: [D b X (Mu b y) y]
      ,  hereMu   :: Mu b y
      }

We can zoom all the way up to the root by repeated plugging in `X` positions.

    muUp :: Diff2 b => MuZpr b y -> Mu b y
    muUp (MuZpr {aboveMu = [], hereMu = t}) = t
    muUp (MuZpr {aboveMu = (dX : dXs), hereMu = t}) =
      muUp (MuZpr {aboveMu = dXs, hereMu = In (up VX (dX :<- XX t))})

But we need *element*-zippers.

<h3>Element-zippers for fixpoints of bifunctors</h3>

Each element is somewhere inside a node. That node is sitting under a stack of `X`-derivatives. But the position of the element in that node is given by a `Y`-derivative. We get

    data MuCx b y = MuCx
      {  aboveY  :: [D b X (Mu b y) y]
      ,  belowY  :: D b Y (Mu b y) y
      }

    instance Diff2 b => Functor (MuCx b) where
      fmap f (MuCx { aboveY = dXs, belowY = dY }) = MuCx
        {  aboveY  = map (bimap (fmap f) f) dXs
        ,  belowY  = bimap (fmap f) f dY
        }

Boldly, I claim

    instance Diff2 b => Diff1 (Mu b) where
      type DF (Mu b) = MuCx b

but before I develop the operations, I'll need some bits and pieces.

I can trade data between functor-zippers and bifunctor-zippers as follows:

    zAboveY :: ZF (Mu b) y -> [D b X (Mu b y) y]  -- the stack of `X`-derivatives above me
    zAboveY (d :<-: y) = aboveY d

    zZipY :: ZF (Mu b) y -> Z b Y (Mu b y) y      -- the `Y`-zipper where I am
    zZipY (d :<-: y) = belowY d :<- YY y

That's enough to let me define:

      upF z  = muUp (MuZpr {aboveMu = zAboveY z, hereMu = In (up VY (zZipY z))})

That is, we go up by first reassembling the node where the element is, turning an element-zipper into a subnode-zipper, then zooming all the way out, as above.

Next, I say

      downF  = yOnDown []

to go down starting with the empty stack, and define the helper function which goes `down` repeatedly from below any stack:

    yOnDown :: Diff2 b => [D b X (Mu b y) y] -> Mu b y -> Mu b (ZF (Mu b) y)
    yOnDown dXs (In b) = In (contextualize dXs (down b))

Now, `down b` only takes us inside the node. The zippers we need must also carry the node's context. That's what `contextualise` does:

    contextualize :: (Bifunctor c, Diff2 b) =>
      [D b X (Mu b y) y] ->
      c (Z b X (Mu b y) y) (Z b Y (Mu b y) y) ->
      c (Mu b (ZF (Mu b) y)) (ZF (Mu b) y)
    contextualize dXs = bimap
      (\ (dX :<- XX t) -> yOnDown (dX : dXs) t)
      (\ (dY :<- YY y) -> MuCx {aboveY = dXs, belowY = dY} :<-: y)

For every `Y`-position, we must give an element-zipper, so it is good we know the whole context `dXs` back to the root, as well as the `dY` which describes how the element sits in its node. For every `X`-position, there is a further subtree to explore, so we grow the stack and keep going!

That leaves only the business of shifting focus. We might stay put, or go down from where we are, or go up, or go up and then down some other path. Here goes.

      aroundF z@(MuCx {aboveY = dXs, belowY = dY} :<-: _) = MuCx
        {  aboveY = yOnUp dXs (In (up VY (zZipY z)))
        ,  belowY = contextualize dXs (cxZ $ around VY (zZipY z))
        }  :<-: z

As ever, the existing element is replaced by its entire zipper. For the `belowY` part, we look where else we can go in the existing node: we will find either alternative element `Y`-positions or further `X`-subnodes to explore, so we `contextualise` them. For the `aboveY` part, we must work our way back up the stack of `X`-derivatives after reassembling the node we were visiting.

    yOnUp :: Diff2 b => [D b X (Mu b y) y] -> Mu b y ->
             [D b X (Mu b (ZF (Mu b) y)) (ZF (Mu b) y)]
    yOnUp [] t = []
    yOnUp (dX : dXs) (t :: Mu b y)
      =  contextualize dXs (cxZ $ around VX (dX :<- XX t))
      :  yOnUp dXs (In (up VX (dX :<- XX t)))

At each step of the way, we can either turn somewhere else that's `around`, or keep going up.

And that's it! I haven't given a formal proof of the laws, but it looks to me as if the operations carefully maintain the context correctly as they crawl the structure.

<h3>What have we learned?</h3>

Differentiability induces notions of thing-in-its-context, inducing a comonadic structure where `extract` gives you the thing and `duplicate` explores the context looking for other things to contextualise. If we have the appropriate differential structure for nodes, we can develop differential structure for whole trees.

Oh, and treating each individual arity of type constructor separately is blatantly horrendous. The better way is to work with functors between indexed sets

    f :: (i -> *) -> (o -> *)

where we make `o` different sorts of structure storing `i` different sorts of element. These are *closed* under the Jacobian construction

    J f :: (i -> *) -> ((o, i) -> *)

where each of the resulting `(o, i)`-structures is a partial derivative, telling you how to make an `i`-element-hole in an `o`-structure. But that's dependently typed fun, for another time.

  [1]: http://strictlypositive.org/CJ.pdf "Clowns and Jokers"
