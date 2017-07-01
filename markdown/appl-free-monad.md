Will this do?

    instance (Functor f) => Applicative (Free f) where
      pure = Return
      Return f  <*> as  = fmap f as
      Roll faf  <*> as  = Roll (fmap (<*> as) faf)

The plan is to act only at the leaves of the tree which produces the function, so for `Return`, we
act by applying the function to all the argument values produced by the argument action. For `Roll`, we just do to all the sub-actions what we intend to do to the overall action.

Crucially, what we do when we reach `Return` is already set before we start. We don't change our plans depending on where we are in the tree. That's the hallmark of being `Applicative`: the structure of the computation is fixed, so that values depend on values but actions don't.
