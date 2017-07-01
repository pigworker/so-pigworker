Reid's right in his comment. When you write

    class Generator g where
      next :: State g a

you're really saying

    class Generator g where
      next :: forall a. State g a

so that from a given state in `g`, your clients can generate an element of whatever type `a` they wish for, rather than whatever type is being supplied by the state in `g`.

There are three sensible ways to fix this problem. I'll sketch them in the order I'd prefer them.

Plan A is to recognize that any generator of things is in some sense a container of them, so presentable as a type constructor rather than a type. It should certainly be a `Functor` and with high probability a `Comonad`. So

    class Comonad f => Generator f where
      move :: forall x. f x -> f x
      next :: forall x. State (f x) x
      next = state $ \ g -> (extract g, move g)
      -- laws
      -- move . duplicate = duplicate . move

    instance Generator [] where
      move = tail

If that's all Greek to you, maybe now is your opportunity to learn some new structure on a need-to-know basis!

Plan B is to ignore the comonadic structure and add an *associated type*.

    class Generator g where
      type From g
      next :: State g (From g)

    instance Generator [a] where
      type From [a] = a
      next = state $ \ (a : as) -> (a, as)

Plan C is the "functional dependencies" version, which is rather like MonadSupply, as suggested by Cirdec.

    class Generator g a | g -> a where
      next :: State g a

    instance Generator [a] a where
      next = state $ \ (a : as) -> (a, as)

What all of these plans have in common is that the functional relationship between `g` and `a` is somehow acknowledged. Without that, there's nothing doing.
