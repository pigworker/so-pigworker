Here's how I'd go about it, using *free* monads. Er, um, what are they? They're trees with actions at the nodes and values at the leaves, with `>>=` acting like tree grafting.

    data f :^* x
      = Ret x
      | Do (f (f :^* x))

It's not unusual to write F<sup>*</sup>X for such a thing in the mathematics, hence my cranky infix type name. To make an instance, you just need `f` to be something you can map over: any `Functor` will do.

    instance Functor f => Monad ((:^*) f) where
      return = Ret
      Ret x  >>= k  = k x
      Do ffx >>= k  = Do (fmap (>>= k) ffx)

That's just "apply `k` at all the leaves and graft in the resulting trees". These can trees represent *strategies* for interactive computation: the whole tree covers every possible interaction with the environment, and the environment chooses which path in the tree to follow. Why are they *free*? They're just trees, with no interesting equational theory on them, saying which strategies are equivalent to which other strategies.

Now let's have a kit for making Functors which correspond to a bunch of commands we might want to be able to do. This thing

    data (:>>:) s t x = s :? (t -> x)

    instance Functor (s :>>: t) where
      fmap k (s :? f) = s :? (k . f)

captures the idea of getting a value in `x` after *one* command with input type `s` and output type `t`. To do that, you need to choose an input in `s` and explain how to continue to the value in `x` given the command's output in `t`. To map a function across such a thing, you tack it onto the continuation. So far, standard equipment. For our problem, we may now define two functors:

    type Modify s  = (s -> s) :>>: ()
    type Yield     = () :>>: ()

It's like I've just written down the value types for the commands we want to be able to do!

Now let's make sure we can offer a *choice* between those commands. We can show that a choice between functors yields a functor. More standard equipment.

    data (:+:) f g x = L (f x) | R (g x)

    instance (Functor f, Functor g) => Functor (f :+: g) where
      fmap k (L fx) = L (fmap k fx)
      fmap k (R gx) = R (fmap k gx)

So, `Modify s :+: Yield` represents the choice between modifying and yielding. Any *signature* of simple commands (communicating with the world in terms of values rather than manipulating computations) can be turned into a functor this way. It's a bother that I have to do it by hand!

That gives me your monad: the free monad over the signature of modify and yield.

    type Pause s = (:^*) (Modify s :+: Yield)

I can define the modify and yield commands as one-do-then-return. Apart from negotiating the dummy input for `yield`, that's just mechanical.

    mutate :: (s -> s) -> Pause s ()
    mutate f = Do (L (f :? Ret))

    yield :: Pause s ()
    yield = Do (R (() :? Ret))

The `step` function then gives a meaning to the strategy trees. It's a *control operator*, constructing one computation (maybe) from another.

    step :: s -> Pause s () -> (s, Maybe (Pause s ()))
    step s (Ret ())            = (s, Nothing)
    step s (Do (L (f  :? k)))  = step (f s) (k ())
    step s (Do (R (() :? k)))  = (s, Just (k ()))

The `step` function runs the strategy until either it finishes with a `Ret`, or it yields, mutating the state as it goes.

The general method goes like this: separate the *commands* (interacting in terms of values) from the *control operators* (manipulating computations); build the free monad of "strategy trees" over the signature of commands (cranking the handle); implement the control operators by recursion over the strategy trees.
