Perhaps the monoid you're looking for is this one.

    newtype AppM f m = AppM (f m) deriving Show

    instance (Applicative f, Monoid m) => Monoid (AppM f m) where
      mempty                      = AppM (pure mempty)
      mappend (AppM fx) (AppM fy) = AppM (pure mappend <*> fx <*> fy)

As a comment, below, observes, it can be found in the [reducers](https://hackage.haskell.org/package/reducers-3.12.1/docs/Data-Semigroup-Applicative.html#t:Ap) library under the name `Ap`. It's fundamental to `Applicative`, so let's unpack it.

Note, in particular, that because `()` is trivially a `Monoid`, `AppM f ()` is a `Monoid`, too. And that's the monoid lurking behind `Applicative f`.

We could have insisted on `Monoid (f ())` as a superclass of `Applicative`, but that would have fouled things up royally.

    > mappend (AppM [(),()]) (AppM [(),(),()])
    AppM [(),(),(),(),(),()]

The monoid underlying `Applicative []` is *multiplication* of natural numbers, whereas the &lsquo;obvious&rsquo; monoidal structure for lists is concatenation, which specialises to *addition* of natural numbers.

**Mathematics warning. Dependent types warning. Fake Haskell warning.**

One way to see what's going on is to consider those Applicatives which happen to be *containers* in the dependently typed sense of Abbott, Altenkirch and Ghani. We'll have these in Haskell sometime soon. I'll just pretend the future has arrived.

    data (<|) (s :: *)(p :: s -> *) (x :: *) where
      (:<|:) :: pi (a :: s) -> (p a -> x) -> (s <| p) x

The data structure `(s <| p)` is characterised by

  * **Shapes** `s` which tell you what the container looks like.
  * **Positions** `p` which tell you *for a given shape* where you can put data.

The above type says that to give data for such a structure is to pick a shape, then fill all the positions with data.

The container presentation of `[]` is `Nat <| Fin` where

    data Nat = Z | S Nat
    data Fin (n :: Nat) where
      FZ :: Fin (S n)
      FS :: Fin n -> Fin (S n)

so that `Fin n` has exactly `n` values. That is, the shape of a list is its *length*, and that tells you how many elements you need to fill up the list.

You can find the shapes for a Haskell `Functor f` by taking `f ()`. By making the data trivial, the positions don't matter. Constructing the GADT of positions generically in Haskell is rather more difficult. 

Parametricity tells us that a polymorphic function between containers in

    forall x. (s <| p) x -> (s' <| p') x

must be given by

  * a function `f :: s -> s'` mapping input shapes to output shapes
  * a function `g :: pi (a :: s) -> p' (f a) -> p a` mapping (for a given input shape) output positions back to the input positions where the output element will come from.

<!- ->

    morph f g (a :<|: d) = f a :<|: (d . g a)

(Secretly, those of us who have had our basic Hancock training also think of "shapes" as "commands" and "positions" as "valid responses". A morphism between containers is then exactly a "device driver". But I digress.)

Thinking along similar lines, what does it take to make a container `Applicative`? For starters,

    pure :: x -> (s <| p) x

which is equivalently

    pure :: (() <| Const ()) x -> (s <| p) x

That has to be given by

    f :: () -> s   -- a constant in s
    g :: pi (a :: ()) -> p (f ()) -> Const () a  -- trivial

where `f = const neutral` for some

    neutral :: s

Now, what about

    (<*>) :: (s <| p) (x -> y) -> (s <| p) x -> (s <| p) y

? Again, parametricity tells us two things. Firstly, the only useful data for calculating the output shapes are the two input shapes. We must have a function

    outShape :: s -> s -> s

Secondly, the only way we can fill an output position with a `y` is to pick a position from the first input to find a function in `x -> y' and then a position in the second input to obtain its argument.

    inPos :: pi (a :: s)(b :: s) -> p (outShape a b) -> (p a, p b)

That is, we can always identify the pair of input positions which determine the output in an output position.

The applicative laws tell us that `neutral` and `outShape` must obey the monoid laws, and that, moreover, we can lift monoids as follows

    mappend (a :<|: f) (b :<|: g) = outShape a b :<|: \ z ->
      let (x, y) = inPos a b z
      in  mappend (f x) (g y)

There's something more to say here, but for that, I need to contrast two operations on containers.

**Composition**

    (s <| p) . (s' <| p')  =  ((s <| p) s') <| \ (a :<|: f) -> Sigma (p a) (p' . f)

where `Sigma` is the type of dependent pairs

    data Sigma (p :: *)(q :: p -> *) where
      Pair :: pi (a :: p) -> q a -> Sigma p q

What on earth does that mean?

  * you choose an outer shape
  * you choose an inner shape for each outer position
  * a composite position is then the pair of an outer position and an inner position appropriate to the inner shape that sits there

Or, in Hancock

  * you choose an outer command
  * you can wait to see the outer response before choosing the inner command
  * a composite response is then a response to the outer command, followed by a response to the inner command chosen by your strategy

Or, more blatantly

  * when you make a list of lists, the inner lists can have different lengths

The `join` of a `Monad` flattens a composition. Lurking behind it is not just a monoid on shapes, but an *integration* operator. That is,

    join :: ((s <| p) . (s <| p)) x -> (s <| p) x

requires

    integrate :: (s <| p) s -> s

Your free monad gives you strategy trees, where you can use the result of one command to choose the rest of your strategy. As if you're interacting at a 1970s teletype.

Meanwhile...

**Tensor**

The tensor (also due to Hancock) of two containers is given by

    (s <| p) >< (s' <| p')  =  (s, s') <| \ (a, b) -> (p a, p' b)

That is

  * you choose two shapes
  * a position is then a pair of positions, one for each shape

or

  * you choose two commands, without seeing any responses
  * a response is then the pair of responses

or

  * `[] >< []` is the type of *rectangular matrices*: the &lsquo;inner&rsquo; lists must all have the same length

The latter is a clue to why `><` is very hard to get your hands on in Haskell, but easy in the dependently typed setting.

Like composition, tensor is a monoid with the identity functor as its neutral element. If we replace the composition underlying `Monad` by tensor, what do we get?

    pure :: Id x -> (s <| p) x
    mystery :: ((s <| p) >< (s <| p)) x -> (s <| p) x

But whatever can `mystery` be? It's not a mystery, because we know there's a rather rigid way to make polymorphic functions between containers. There must be

    f :: (s, s) -> s
    g :: pi ((a, b) :: (s, s)) -> p (f (a, b)) -> (p a, p b)

and those are exactly what we said determined `<*>` earlier.

`Applicative` is the notion of effectful programming generated by tensor, where `Monad` is generated by composition. The fact that you don't get to/need to wait for the outer response to choose the inner command is why `Applicative` programs are more readily parallelizable.

Seeing `[] >< []` as rectangular matrices tells us why `<*>` for lists is built on top of multiplication.

The free applicative functor is the free monoid with knobs on. For containers,

    Free (s <| p) = [s] <| All p

where

    All p [] = ()
    All p (x : xs) = (p x, All p xs)

So a "command" is a big list of commands, like a deck of punch cards. You don't get to see any output before you choose your card deck. The "response" is your lineprinter output. It's the 1960s.

So there you go. The very nature of `Applicative`, tensor not composition, demands an underlying monoid, and a recombination of elements compatible with monoids.
