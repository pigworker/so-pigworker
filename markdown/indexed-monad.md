As ever, the terminology people use is not entirely consistent. There's a variety of inspired-by-monads-but-strictly-speaking-isn't-quite notions. The term "indexed monad" is one of a number (including "monadish" and "parameterised monad" (Atkey's name for them)) of terms used to characterize one such notion. (Another such notion, if you're interested, is Katsumata's "parametric effect monad", indexed by a monoid, where return is indexed neutrally and bind accumulates in its index.)

First of all, let's check kinds.

    IxMonad (m :: state -> state -> * -> *)

That is, the type of a "computation" (or "action", if you prefer, but I'll stick with "computation"), looks like

    m before after value

where `before, after :: state` and `value :: *`. The idea is to capture the means to interact safely with an external system that has some *predictable* notion of state. A computation's type tells you what the state must be `before` it runs, what the state will be `after` it runs and (like with regular monads over `*`) what type of `value`s the computation produces.

The usual bits and pieces are `*`-wise like a monad and `state`-wise like playing dominoes.

    ireturn  ::  a -> m i i a    -- returning a pure value preserves state
    ibind    ::  m i j a ->      -- we can go from i to j and get an a, thence
                 (a -> m j k b)  -- we can go from j to k and get a b, therefore
                 -> m i k b      -- we can indeed go from i to k and get a b

The notion of "Kleisli arrow" (function which yields computation) thus generated is

    a -> m i j b   -- values a in, b out; state transition i to j

and we get a composition

    icomp :: IxMonad m => (b -> m j k c) -> (a -> m i j b) -> a -> m i k c
    icomp f g = \ a -> ibind (g a) f

and, as ever, the laws exactly ensure that `ireturn` and `icomp` give us a category

          ireturn `icomp` g = g
          f `icomp` ireturn = f
    (f `icomp` g) `icomp` h = f `icomp` (g `icomp` h)

or, in comedy fake C/Java/whatever,

          g(); skip = g()
          skip; f() = f()
    {h(); g()}; f() = h(); {g(); f()}

Why bother? To model "rules" of interaction. For example, you can't eject a dvd if there isn't one in the drive, and you can't put a dvd into the drive if there's one already in it. So

    data DVDDrive :: Bool -> Bool -> * -> * where  -- Bool is "drive full?"
      DReturn :: a -> DVDDrive i i a
      DInsert :: DVD ->                   -- you have a DVD
                 DVDDrive True k a ->     -- you know how to continue full
                 DVDDrive False k a       -- so you can insert from empty
      DEject  :: (DVD ->                  -- once you receive a DVD
                  DVDDrive False k a) ->  -- you know how to continue empty
                 DVDDrive True k a        -- so you can eject when full

    instance IxMonad DVDDrive where  -- put these methods where they need to go
      ireturn = DReturn              -- so this goes somewhere else
      ibind (DReturn a)     k  = k a
      ibind (DInsert dvd j) k  = DInsert dvd (ibind j k)
      ibind (DEject j)      k  = DEject j $ \ dvd -> ibind (j dvd) k

With this in place, we can define the "primitive" commands

    dInsert :: DVD -> DVDDrive False True ()
    dInsert dvd = DInsert dvd $ DReturn ()

    dEject :: DVDrive True False DVD
    dEject = DEject $ \ dvd -> DReturn dvd

from which others are assembled with `ireturn` and `ibind`. Now, I can write (borrowing `do`-notation)

    discSwap :: DVD -> DVDDrive True True DVD
    discSwap dvd = do dvd' <- dEject; dInsert dvd ; ireturn dvd'

but not the physically impossible

    discSwap :: DVD -> DVDDrive True True DVD
    discSwap dvd = do dInsert dvd; dEject      -- ouch!

Alternatively, one can define one's primitive commands directly

    data DVDCommand :: Bool -> Bool -> * -> * where
      InsertC  :: DVD -> DVDCommand False True ()
      EjectC   :: DVDCommand True False DVD

and then instantiate the generic template

    data CommandIxMonad :: (state -> state -> * -> *) ->
                            state -> state -> * -> * where
      CReturn  :: a -> CommandIxMonad c i i a
      (:?)     :: c i j a -> (a -> CommandIxMonad c j k b) ->
                    CommandIxMonad c i k b

    instance IxMonad (CommandIxMonad c) where
      ireturn = CReturn
      ibind (CReturn a) k  = k a
      ibind (c :? j)    k  = c :? \ a -> ibind (j a) k

In effect, we've said what the primitive Kleisli arrows are (what one "domino" is), then built a suitable notion of "computation sequence" over them.

Note that for every indexed monad `m`, the "no change diagonal" `m i i` is a monad, but in general, `m i j` is not. Moreover, values are not indexed but computations are indexed, so an indexed monad is not just the usual idea of monad instantiated for some other category.

Now, look again at the type of a Kleisli arrow

    a -> m i j b

We know we must be in state `i` to start, and we predict that any continuation will start from state `j`. We know a lot about this system! This isn't a risky operation! When we put the dvd in the drive, it goes in! The dvd drive doesn't get any say in what the state is after each command.

But that's not true in general, when interacting with the world. Sometimes you might need to give away some control and let the world do what it likes. For example, if you are a server, you might offer your client a choice, and your session state will depend on what they choose. The server's "offer choice" operation does not determine the resulting state, but the server should be able to carry on anyway. It's not a "primitive command" in the above sense, so indexed monads are not such a good tool to model the *unpredictable* scenario.

What's a better tool?

    type f :-> g = forall state. f state -> g state

    class MonadIx (m :: (state -> *) -> (state -> *)) where
      returnIx    :: x :-> m x
      flipBindIx  :: (a :-> m b) -> (m a :-> m b)  -- tidier than bindIx

Scary biscuits? Not really, for two reasons. One, it looks rather more like what a monad is, because it *is* a monad, but over `(state -> *)` rather than `*`. Two, if you look at the type of a Kleisli arrow,

    a :-> m b   =   forall state. a state -> m b state

you get the type of computations with a *precondition* `a` and postcondition `b`, just like in Good Old Hoare Logic. Assertions in program logics have taken under half a century to cross the Curry-Howard correspondence and become Haskell types. The type of `returnIx` says "you can achieve any postcondition which holds, just by doing nothing", which is the Hoare Logic rule for "skip". The corresponding composition is the Hoare Logic rule for ";".

Let's finish by looking at the type of `bindIx`, putting all the quantifiers in.

    bindIx :: forall i. m a i -> (forall j. a j -> m b j) -> m b i

These `forall`s have opposite polarity. We choose initial state `i`, and a computation which can start at `i`, with postcondition `a`. The world chooses any intermediate state `j` it likes, but it must give us the evidence that postcondition `b` holds, and from any such state, we can carry on to make `b` hold. So, in sequence, we can achieve condition `b` from state `i`. By releasing our grip on the "after" states, we can model *unpredictable* computations.

Both `IxMonad` and `MonadIx` are useful. Both model validity of interactive computations with respect to changing state, predictable and unpredictable, respectively. Predictability is valuable when you can get it, but unpredictability is sometimes a fact of life. Hopefully, then, this answer gives some indication of what indexed monads are, predicting both when they start to be useful and when they stop.
