Here's the essence of a solution. I've switched on

    {-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, PatternSynonyms #-}

Let's just recap fixpoints and catamorphisms.

    newtype Fix f = In {out :: f (Fix f)}

    cata :: Functor f => (f t -> t) -> Fix f -> t
    cata alg = alg . fmap (cata alg) . out

The algebra, `alg :: f t -> t`, takes a node where the children have already been replaced by a `t` value, then returns the `t` for the parent. The `cata` operator works by unpacking the parent node, processing all its children recursively, then applying `alg` to finish the job.

So, if we want to count leaves in such a structure, we can start like this:

    leaves :: (Foldable f, Functor f) => Fix f -> Integer
    leaves = cata sumOrOne where
      -- sumOrOne :: f Integer -> Integer

The algebra, `sumOrOne` can see the number of leaves in each child of the parent node. We can use `cata` because `f` is a `Functor`. And because `f` is `Foldable`, we can compute the total number of leaves in the children.

      sumOrOne fl = case sum fl of
        ...

There are then two possibilities: if the parent has no children, its leaf sum will be `0`, which we can detect, but that means the parent is itself a leaf, so `1` should be returned. Otherwise, the leaf sum will be nonzero, in which case the parent is not a leaf, so its leaf sum is indeed the total leaf sum of its children. That gives us

    leaves :: (Foldable f, Functor f) => Fix f -> Integer
    leaves = cata sumOrOne where
      sumOrOne fl{- number of leaves in each child-} = case sum fl of
        0 -> 1  -- no leaves in my children means I am a leaf
        l -> l  -- otherwise, pass on the total

A quick example, based on Hutton's Razor (the expression language with integers and addition, which is often the simplest thing that illustrates the point). The expressions are generated from Hutton's functor.

    data HF h = Val Int | h :+: h deriving (Functor, Foldable, Traversable)

I introduce some pattern synonyms to recover the look and feel of a bespoke type.

    pattern V x    = In (Val x)
    pattern s :+ t = In (s :+: t)

I cook up a quick example expression, with some leaves that are three levels deep.

    example :: Fix HF
    example = (V 1 :+ V 2) :+ ((V 3 :+ V 4) :+ V 5)

Sure enough

    Ok, modules loaded: Leaves.
    *Leaves> leaves example
    5

An alternative approach is to be functorial and foldable in substructures of interest, in this case, stuff at leaves. (We get exactly the free monads.)

    data Tree f x = Leaf x | Node (f (Tree f x)) deriving (Functor, Foldable)

Once you've made the leaf/node separation part of your basic construction, you can visit the leaves directly with `foldMap`. Throwing in a bit of `Control.Newtype`, we get

    ala' Sum foldMap (const 1)  :: Foldable f => f x -> Integer

which is below the Fairbairn Threshold (i.e., short enough not to need a name and all the clearer for not having one).

The trouble, of course, is that data structures are often functorial in "substructures of interest" in multiple interesting but conflicting ways. Haskell isn't always the best at letting us access "found functoriality": we somehow have to predict the functoriality we need when we parametrise a data type at declaration time. But there's still time to change all that...
