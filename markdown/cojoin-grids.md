**Jagger/Richards: you can't always get what you want, but if you try sometime you just might find that you get what you need.**

## Cursors in Lists ##

Let me rebuild the components of your structure using snoc- and cons-lists to keep the spatial properties clear. I define

    data Bwd x = B0 | Bwd x :< x deriving (Functor, Foldable, Traversable, Show)
    data Fwd x = F0 | x :> Fwd x deriving (Functor, Foldable, Traversable, Show)
    infixl 5 :<
    infixr 5 :>

    data Cursor x = Cur (Bwd x) x (Fwd x) deriving (Functor, Foldable, Traversable, Show)

Let's have comonads

    class Functor f => Comonad f where
      counit  :: f x -> x
      cojoin  :: f x -> f (f x)

and let's make sure cursors are comonads

    instance Comonad Cursor where
      counit (Cur _ x _) = x
      cojoin c = Cur (lefts c) c (rights c) where
        lefts (Cur B0 _ _) = B0
        lefts (Cur (xz :< x) y ys) = lefts c :< c where c = Cur xz x (y :> ys)
        rights (Cur _ _ F0) = F0
        rights (Cur xz x (y :> ys)) = c :> rights c where c = Cur (xz :< x) y ys

If you're turned on to this kind of stuff, you'll note that `Cursor` is a spatially pleasing variant of `InContext []`

    InContext f x = (x, ∂f x)

where ∂ takes the formal derivative of a functor, giving its notion of one-hole context. `InContext f` is always a `Comonad`, as mentioned in [this answer][1], and what we have here is just that `Comonad` induced by the differential structure, where `counit` extracts the element at the focus and `cojoin` decorates each element with its own context, effectively giving you a context full of refocused cursors and with an unmoved cursor at its focus. Let's have an example.

    > cojoin (Cur (B0 :< 1) 2 (3 :> 4 :> F0))
    Cur (B0 :< Cur B0 1 (2 :> 3 :> 4 :> F0))
        (Cur (B0 :< 1) 2 (3 :> 4 :> F0))
        (  Cur (B0 :< 1 :< 2) 3 (4 :> F0)
        :> Cur (B0 :< 1 :< 2 :< 3) 4 F0
        :> F0)

See? The 2 in focus has been decorated to become the cursor-at-2; to the left, we have the list of the cursor-at-1; to the right, the list of the cursor-at-3 and the cursor-at-4.

## Composing Cursors, Transposing Cursors? ##

Now, the structure you're asking to be a `Comonad` is the n-fold composition of `Cursor`. Let's have

    newtype (:.:) f g x = C {unC :: f (g x)} deriving Show

To persuade comonads `f` and `g` to compose, the `counit`s compose neatly, but you need a "distributive law"

    transpose :: f (g x) -> g (f x)

so you can make the composite `cojoin` like this

    f (g x)
      -(fmap cojoin)->
    f (g (g x))
      -cojoin->
    f (f (g (g x)))
      -(fmap transpose)->
    f (g (f (g x)))

What laws should `transpose` satisfy? Probably something like

    counit . transpose = fmap counit
    cojoin . transpose = fmap transpose . transpose . fmap cojoin

or whatever it takes to ensure that any two ways to shoogle some sequence of f's and g's from one order to another give the same result.

Can we define a `transpose` for `Cursor` with itself? One way to get some sort of transposition cheaply is to note that `Bwd` and `Fwd` are *zippily* applicative, hence so is `Cursor`.

    instance Applicative Bwd where
      pure x = pure x :< x
      (fz :< f) <*> (sz :< s) = (fz <*> sz) :< f s
      _ <*> _ = B0

    instance Applicative Fwd where
      pure x = x :> pure x
      (f :> fs) <*> (s :> ss) = f s :> (fs <*> ss)
      _ <*> _ = F0

    instance Applicative Cursor where
      pure x = Cur (pure x) x (pure x)
      Cur fz f fs <*> Cur sz s ss = Cur (fz <*> sz) (f s) (fs <*> ss)

And here you should begin to smell the rat. Shape mismatch results in *truncation*, and that's going to break the obviously desirable property that self-transpose is self-inverse. Any kind of raggedness will not survive. We do get a transposition operator: `sequenceA`, and for completely regular data, all is bright and beautiful.

    > regularMatrixCursor
    Cur (B0 :< Cur (B0 :< 1) 2 (3 :> F0))
              (Cur (B0 :< 4) 5 (6 :> F0))
              (Cur (B0 :< 7) 8 (9 :> F0) :> F0)
    > sequenceA regularMatrixCursor
    Cur (B0 :< Cur (B0 :< 1) 4 (7 :> F0))
              (Cur (B0 :< 2) 5 (8 :> F0))
              (Cur (B0 :< 3) 6 (9 :> F0) :> F0)

But even if I just move one of the inner cursors out of alignment (never mind making the sizes ragged), things go awry.

    > raggedyMatrixCursor
    Cur (B0 :< Cur ((B0 :< 1) :< 2) 3 F0)
              (Cur (B0 :< 4) 5 (6 :> F0))
              (Cur (B0 :< 7) 8 (9 :> F0) :> F0)
    > sequenceA raggedyMatrixCursor
    Cur (B0 :< Cur (B0 :< 2) 4 (7 :> F0))
              (Cur (B0 :< 3) 5 (8 :> F0))
              F0

When you have one outer cursor position and multiple inner cursor positions, there's no transposition which is going to behave well. Self-composing `Cursor` allows the inner structures to be ragged relative to one another, so no `transpose`, no `cojoin`. You can, and I did, define

    instance (Comonad f, Traversable f, Comonad g, Applicative g) =>
      Comonad (f :.: g) where
        counit = counit . counit . unC
        cojoin = C . fmap (fmap C . sequenceA) . cojoin . fmap cojoin . unC

but it's an onus on us to make sure we keep the inner structures regular. If you're willing to accept that burden, then you can iterate, because `Applicative` and `Traversable` are readily closed under composition. Here are the bits and pieces

    instance (Functor f, Functor g) => Functor (f :.: g) where
      fmap h (C fgx) = C (fmap (fmap h) fgx)

    instance (Applicative f, Applicative g) => Applicative (f :.: g) where
      pure = C . pure . pure
      C f <*> C s = C (pure (<*>) <*> f <*> s)

    instance (Functor f, Foldable f, Foldable g) => Foldable (f :.: g) where
      fold = fold . fmap fold . unC

    instance (Traversable f, Traversable g) => Traversable (f :.: g) where
      traverse h (C fgx) = C <$> traverse (traverse h) fgx

**Edit:** for completeness, here's what it does when all is regular,

    > cojoin (C regularMatrixCursor)
    C {unC = Cur (B0 :< Cur (B0 :<
      C {unC = Cur B0 (Cur B0 1 (2 :> (3 :> F0))) (Cur B0 4 (5 :> (6 :> F0)) :> (Cur B0 7 (8 :> (9 :> F0)) :> F0))}) 
     (C {unC = Cur B0 (Cur (B0 :< 1) 2 (3 :> F0)) (Cur (B0 :< 4) 5 (6 :> F0) :> (Cur (B0 :< 7) 8 (9 :> F0) :> F0))})
     (C {unC = Cur B0 (Cur ((B0 :< 1) :< 2) 3 F0) (Cur ((B0 :< 4) :< 5) 6 F0 :> (Cur ((B0 :< 7) :< 8) 9 F0 :> F0))} :> F0))
    (Cur (B0 :<
      C {unC = Cur (B0 :< Cur B0 1 (2 :> (3 :> F0))) (Cur B0 4 (5 :> (6 :> F0))) (Cur B0 7 (8 :> (9 :> F0)) :> F0)})
     (C {unC = Cur (B0 :< Cur (B0 :< 1) 2 (3 :> F0)) (Cur (B0 :< 4) 5 (6 :> F0)) (Cur (B0 :< 7) 8 (9 :> F0) :> F0)}) 
     (C {unC = Cur (B0 :< Cur ((B0 :< 1) :< 2) 3 F0) (Cur ((B0 :< 4) :< 5) 6 F0) (Cur ((B0 :< 7) :< 8) 9 F0 :> F0)} :> F0))
    (Cur (B0 :<
      C {unC = Cur ((B0 :< Cur B0 1 (2 :> (3 :> F0))) :< Cur B0 4 (5 :> (6 :> F0))) (Cur B0 7 (8 :> (9 :> F0))) F0})
     (C {unC = Cur ((B0 :< Cur (B0 :< 1) 2 (3 :> F0)) :< Cur (B0 :< 4) 5 (6 :> F0)) (Cur (B0 :< 7) 8 (9 :> F0)) F0})
     (C {unC = Cur ((B0 :< Cur ((B0 :< 1) :< 2) 3 F0) :< Cur ((B0 :< 4) :< 5) 6 F0) (Cur ((B0 :< 7) :< 8) 9 F0) F0} :> F0)
    :> F0)}

## Hancock's Tensor Product ##

For regularity, you need something stronger than composition. You need to be able to capture the notion of "an f-structure of g-structures-all-the-same-shape". This is what the inestimable Peter Hancock calls the "tensor product", which I'll write `f :><: g`: there's one "outer" f-shape and one "inner" g-shape common to all the inner g-structures, so transposition is readily definable and always self-inverse. Hancock's tensor is not conveniently definable in Haskell, but in a dependently typed setting, it's easy to formulate a notion of "container" which has this tensor.

To give you the idea, consider a degenerate notion of container

    data (:<|) s p x = s :<| (p -> x)

where we say `s` is the type of "shapes" and `p` the type of "positions". A value consists of the choice of a shape and the storage of an `x` in each position. In the dependent case, the type of positions may depend on the choice of the shape (e.g., for lists, the shape is a number (the length), and you have that many positions). These containers have a tensor product

    (s :<| p) :><: (s' :<| p')  =  (s, s') :<| (p, p')

which is like a generalised matrix: a pair of shapes give the dimensions, and then you have an element at each pair of positions. You can do this perfectly well when types `p` and `p'` depend on values in `s` and `s'`, and that is exactly Hancock's definition of the tensor product of containers.

## InContext for Tensor Products ##

Now, as you may have learned in high school, `∂(s :<| p) = (s, p) :<| (p-1)` where `p-1` is some type with one fewer element than `p`. Like ∂(s*x^p) = (s*p)*x^(p-1). You select one position (recording it in the shape) and delete it. The snag is that `p-1` is tricky to get your hands on without dependent types. But `InContext` selects a position *without deleting it*.

    InContext (s :<| p) ~= (s, p) :<| p

This works just as well for the dependent case, and we joyously acquire

    InContext (f :><: g) ~= InContext f :><: InContext g

Now we know that `InContext f` is always a `Comonad`, and this tells us that tensor products of `InContext`s are comonadic because they are themselves `InContext`s. That's to say, you pick one position per dimension (and that gives you exactly one position in the whole thing), where before we had one outer position and lots of inner positions. With the tensor product replacing composition, everything works sweetly.

## Naperian Functors ##

But there is a subclass of `Functor` for which the tensor product and the composition coincide. These are the `Functor`s `f` for which `f () ~ ()`: i.e., there is only one shape anyway, so raggedy values in compositions are ruled out in the first place. These `Functor`s are all isomorphic to `(p ->)` for some position set `p` which we can think of as the *logarithm* (the exponent to which `x` must be raised to give `f x`). Correspondingly, Hancock calls these `Naperian` functors after John Napier (whose ghost haunts the part of Edinburgh where Hancock lives).

    class Applicative f => Naperian f where
      type Log f
      project :: f x -> Log f -> x
      positions :: f (Log f)
      --- project positions = id

A `Naperian` functor has a logarithm, inducing a `project`ion function mapping positions to the elements found there. `Naperian` functors are all zippily `Applicative`, with `pure` and `<*>` corresponding to the K and S combinators for the projections. It's also possible to construct a value where at each position is stored that very position's representation. Laws of logarithms which you might remember pop up pleasingly.

    newtype Id x = Id {unId :: x} deriving Show

    instance Naperian Id where
      type Log Id = ()
      project (Id x) () = x
      positions = Id ()

    newtype (:*:) f g x = Pr (f x, g x) deriving Show

    instance (Naperian f, Naperian g) => Naperian (f :*: g) where
      type Log (f :*: g) = Either (Log f) (Log g)
      project (Pr (fx, gx)) (Left p) = project fx p
      project (Pr (fx, gx)) (Right p) = project gx p
      positions = Pr (fmap Left positions, fmap Right positions)

Note that a fixed size array (a *vector*) is given by `(Id :*: Id :*: ... :*: Id :*: One)`, where `One` is the constant unit functor, whose logarithm is `Void`. So an array is `Naperian`. Now, we also have

    instance (Naperian f, Naperian g) => Naperian (f :.: g) where
      type Log (f :.: g) = (Log f, Log g)
      project (C fgx) (p, q) = project (project fgx p) q
      positions = C $ fmap (\ p -> fmap (p ,) positions) positions

which means that multi-dimensional arrays are `Naperian`.

To construct a version of `InContext f` for `Naperian f`, just point to a position!

    data Focused f x = f x :@ Log f

    instance Functor f => Functor (Focused f) where
      fmap h (fx :@ p) = fmap h fx :@ p

    instance Naperian f => Comonad (Focused f) where
      counit (fx :@ p) = project fx p
      cojoin (fx :@ p) = fmap (fx :@) positions :@ p

So, in particular, a `Focused` n-dimensional array will indeed be a comonad. A composition of vectors is a tensor product of n vectors, because vectors are `Naperian`. But the `Focused` n-dimensional array will be the n-fold tensor product, *not the composition*, of the n `Focused` vectors which determine its dimensions. To express this comonad in terms of zippers, we'll need to express them in a form which makes it possible to construct the tensor product. I'll leave that as an exercise for the future.

  [1]: https://stackoverflow.com/a/12872133/828361
