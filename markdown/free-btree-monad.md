One way to define a `Monad` instance for this type is to treat it as a *free* monad. In effect, this takes `A a` to be a little syntax with one binary operator `C`, and variables represented by values of type `a` embedded by the `B` constructor. That makes `return` the `B` constructor, embedding variables, and `>>=` the operator which performs subsitution.

    instance Monad A where
      return = B
      B x   >>= f = f x
      C l r >>= f = C (l >>= f) (r >>= f)

It's not hard to see that `(>>= B)` performs the identity substitution, and that composition of substitutions is associative.

Another, more "imperative" way to see this monad is that it captures the idea of computations that can flip coins (or read a bitstream or otherwise have some access to a sequence of binary choices).

    data Coin = Heads | Tails

Any computation which can flip coins must either stop flipping and be a value (with `B`), or flip a coin and carry on (with `C`) in one way if the coin comes up `Heads` and another if `Tails`. The monadic operation which flips a coin and tells you what came up is

    coin :: A Coin
    coin = C (B Heads) (B Tails)

The `>>=` of `A` can now be seen as sequencing coin-flipping computations, allowing the choice of a subsequent computation to depend on the value delivered by an earlier computation.

If you have an infinite stream of coins, then (apart from your extraordinary good fortune) you're also lucky enough to be able to run any `A`-computation to its value, as follows

    data Stream x = x :> Stream x   -- actually, I mean "codata"

    flipping :: Stream Coin -> A v -> v
    flipping _             (B v)    = v
    flipping (Heads :> cs) (C h t)  = flipping cs h
    flipping (Tails :> cs) (C h t)  = flipping cs t

The general pattern in this sort of monad is to have one constructor for returning a value (`B` here) and a bunch of others which represent the choice of possible operations and the different ways computations can continue given the result of an operation. Here `C` has no non-recursive parameters and two subtrees, so I could tell that there must be just one operation and that it must have just two possible outcomes, hence flipping a coin.

So, it's substitution for a syntax with variables and one binary operator, or it's a way of sequencing computations that flip coins. Which view is better? Well... they're two sides of the same coin.
