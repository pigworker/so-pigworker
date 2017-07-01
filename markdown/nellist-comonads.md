Nonempty lists arise as two distinct comonads by two standard constructions.

Firstly, the *cofree comonad* is given thus.

    data Cofree f x = x :& f (Cofree f x)  -- every node is labelled with an x

    instance Functor f => Functor (Cofree f) where
      fmap f (x :& fcx) = f x :& fmap (fmap f) fcx

    instance Functor f => Comonad (Cofree f) where
      extract (x :& _) = x   -- get the label of the top node
      duplicate cx@(_ :& fcx) = cx :& fmap duplicate fcx

Nonempty lists can be given as

    type Nellist1 = Cofree Maybe

and are thus automatically comonadic. That gives you the "tails" comonad.

Meanwhile, the decomposition of a structure as an "element zipper" induces comonadic structure. As I [explained at great length](https://stackoverflow.com/questions/25554062/zipper-comonads-generically),

Differentiability amounts to this bunch of operations on zippers (individual elements picked out of their context and put "in focus")

    class (Functor f, Functor (DF f)) => Diff1 f where
      type DF f :: * -> *
      upF      ::  ZF f x  ->  f x           -- defocus
      downF    ::  f x     ->  f (ZF f x)    -- find all ways to focus
      aroundF  ::  ZF f x  ->  ZF f (ZF f x) -- find all ways to *re*focus

    data ZF f x = (:<-:) {cxF :: DF f x, elF :: x}

so we get a functor and a comonad

    instance Diff1 f => Functor (ZF f) where
      fmap f (df :<-: x) = fmap f df :<-: f x

    instance Diff1 f => Comonad (ZF f) where
      extract    = elF
      duplicate  = aroundF

In principle, nonempty lists arise by this construction, too. The trouble is that the functor being differentiated is not so easy to express in Haskell, even though the derivative is sensible. Let's go nuts...

Nonempty lists amount to `ZF thingy x` where `DF thingy = []`. Can we integrate lists? Fooling about algebraically might give us a clue

    [x] = Either () (x, [x]) = 1 + x * [x]

so as a power series, we get

    [x] = Sum(n :: Nat). x^n

and we can integrate power series

    Integral [x] dx = Sum(n :: Nat). x^(n+1)/(n+1)

which means we get some sort of arbitrary tuples of size (n+1), but we have to identify them up to some relation where the equivalence classes have size (n+1). One way to do that is to identify tuples up to rotation, so you don't know which of the (n+1) positions is "first".

That is, lists are the derivative of nonempty cycles. Think about a bunch of people at a round table playing cards (possibly solitaire). Rotate the table and you get the same bunch of people playing cards. But once you designate the *dealer*, you fix the list of other players in order, clockwise starting left of the dealer.

Two standard constructions; two comonads for the same functor.

(In my comment earlier, I remarked about the possibility of multiple monads. It's a bit involved, but here's a starting point. Every monad `m` is also applicative, and the applicative laws make `m ()` a monoid. Correspondingly, every monoid structure for `m ()` at least gives a candidate for a monad structure on `m`. In the case of writer monads `(,) s`, we get that the candidates for monads are the monoids on `(s,())` which are just the same as the monoids on `s`.)

**Edit** Nonempty lists are also *monadic* in at least two distinct ways.

I define the identity and pairing for functors, as follows.

    newtype I         x = I x
    data    (f :*: g) x = (:&:) {lll :: f x, rrr :: g x}

Now, I can introduce nonempty lists as follows, then define concatenation.

    newtype Ne x = Ne ((I :*: []) x)

    cat :: Ne x -> Ne x -> Ne x
    cat (Ne (I x :&: xs)) (Ne (I y :&: ys)) = Ne (I x :&: (xs ++ y : ys))

These are monadic just the way possibly empty lists are:

    instance Monad Ne where
      return x = Ne (I x :&: [])
      Ne (I x :&: xs) >>= k = foldl cat (k x) (map k xs)

However, `I` is a monad:

    instance Monad I where
      return = I
      I a >>= k = k a

Moreover, monads are closed under pairing:

    instance (Monad f, Monad g) => Monad (f :*: g) where
      return x = return x :&: return x
      (fa :&: ga) >>= k = (fa >>= (lll . k)) :&: (ga >>= (rrr . k))

So we could just have written

    newtype Ne x = Ne ((I :*: []) x) deriving (Monad, Applicative, Functor)

but the `return` for that monad gives us double vision.

    return x = Ne (I x :&: [x])

So there you are: nonempty lists are comonadic two ways, monadic two ways, applicative six ways,...

(Lots more to say about this, but I have to stop somewhere.)
