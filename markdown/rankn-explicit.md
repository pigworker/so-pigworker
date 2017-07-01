Let's be bloody. We must quantify everything and give the domain of quantification. Values have types; type-level things have kinds; kinds live in `BOX`.

    f1 :: forall (k :: BOX).
          (forall (a :: k) (m :: k -> *). m a -> Int)
          -> Int

    f2 :: (forall (k :: BOX) (a :: k) (m :: k -> *). m a -> Int)
          -> Int

Now, in neither example type is `k` quantified explicitly, so ghc is deciding where to put that `forall (k :: BOX)`, based on whether and where `k` is mentioned. I am not totally sure I understand or am willing to defend the policy as stated.

Ørjan gives a good example of the difference in practice. Let's be bloody about that, too. I'll write `/\ (a :: k). t` to make explicit the abstraction that corresponds to `forall`, and `f @ type` for the corresponding application. The game is that we get to pick the `@`-ed arguments, but we have to be ready to put up with whatever `/\`-ed arguments the devil may choose.

We have

    x :: forall (a :: *) (m :: * -> *). m a -> Int

and may accordingly discover that `f1 x` is really

    f1 @ * (/\ (a :: *) (m :: * -> *). x @ a @ m)

However, if we try to give `f2 x` the same treatment, we see

    f2 (/\ (k :: BOX) (a :: k) (m :: k -> *). x @ ?m0 @ ?a0)
    ?m0 :: *
    ?a0 :: * -> *
    where  m a = m0 a0

The Haskell type system treats type application as purely syntactic, so the only way that equation can be solved is by identifying the functions and identifying the arguments

    (?m0 :: * -> *) = (m :: k -> *)
    (?a0 :: *)      = (a :: k)

but those equations are not even well kinded, because `k` is not free to be chosen: it's being `/\`-ed not `@`-ed.

Generally, to get to grips with these uber-polymorphic types, it's good to write out all the quantifiers and then figure out how that turns into your game against the devil. Who chooses what, and in what order. Moving a `forall` inside an argument type changes its chooser, and can often make the difference between victory and defeat.
