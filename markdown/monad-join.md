Without plumbing the depths of metaphor, might I suggest to read a typical monad `m` as "strategy to produce a", so the type `m value` is a first class "strategy to produce a value". Different notions of computation or external interaction require different types of strategy, but the general notion requires some regular structure to make sense:

 - if you already have a value, then you have a strategy to produce a value (`return :: v -> m v`) consisting of nothing other than producing the value that you have;
 - if you have a function which transforms one sort of value into another, you can lift it to strategies (`fmap :: (v -> u) -> m v -> m u`) just by waiting for the strategy to deliver its value, then transforming it;
 - if you have a strategy to produce a strategy to produce a value, then you can construct a strategy to produce a value (`join :: m (m v) -> m v`) which follows the outer strategy until it produces the inner strategy, then follows that inner strategy all the way to a value.

Let's have an example: leaf-labelled binary trees...

    data Tree v = Leaf v | Node (Tree v) (Tree v)

...represent strategies to produce stuff by tossing a coin. If the strategy is `Leaf v`, there's your `v`; if the strategy is `Node h t`, you toss a coin and continue by strategy `h` if the coin shows "heads", `t` if it's "tails".

    instance Monad Tree where
      return = Leaf

A strategy-producing strategy is a tree with tree-labelled leaves: in place of each such leaf, we can just graft in the tree which labels it...

      join (Leaf tree) = tree
      join (Node h t)  = Node (join h) (join t)

...and of course we have `fmap` which just relabels leaves.

    instance Functor Tree where
      fmap f (Leaf x)    = Leaf (f x)
      fmap f (Node h t)  = Node (fmap f h) (fmap f t)

Here's an strategy to produce a strategy to produce an `Int`.

![tree of trees][1]

Toss a coin: if it's "heads", toss another coin to decide between two strategies (producing, respectively, "toss a coin for producing 0 or producing 1" or "produce 2"); if it's "tails" produce a third ("toss a coin for producing 3 or tossing a coin for 4 or 5").

That clearly `join`s up to make a strategy producing an `Int`.

 ![enter image description here][2]

What we're making use of is the fact that a "strategy to produce a value" can itself be seen as a value. In Haskell, the embedding of strategies as values is silent, but in English, I use quotation marks to distinguish using a strategy from just talking about it. The `join` operator expresses the strategy "somehow produce then follow a strategy", or "if you are *told* a strategy, you may then *use* it".

(Meta. I'm not sure whether this "strategy" approach is a suitably generic way to think about monads and the value/computation distinction, or whether it's just another crummy metaphor. I do find leaf-labelled tree-like types a useful source of intuition, which is perhaps not a surprise as they're the *free* monads, with just enough structure to be monads at all, but no more.)

PS The type of "bind"

    (>>=) :: m v -> (v -> m w) -> m w

says "if you have a strategy to produce a `v`, and for each v a follow-on strategy to produce a `w`, then you have a strategy to produce a `w`". How can we capture that in terms of `join`?

    mv >>= v2mw = join (fmap v2mw mv)

We can relabel our `v`-producing strategy by `v2mw`, producing instead of each `v` value the `w`-producing strategy which follows on from it &mdash; ready to `join`!

  [1]: http://i.stack.imgur.com/7ts7a.jpg
  [2]: http://i.stack.imgur.com/Dw5vO.jpg
