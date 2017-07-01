It is well observed in the comments that your `otherwise` (which is just a synonym for `True`) needs an `=` sign like any other guard, but I'd make a few other adjustments.

Partial functions `head` and `tail` are probably better avoided, especially as there is a good way to solve this problem with pattern matching.

    elementBefore :: Eq a => a -> [a] -> Maybe a
    elementBefore elt (x : xs@(y : _)) | y == elt = Just x
                                       | otherwise = elementBefore elt xs
    elementBefore _ _ = Nothing

The key is the use of `@` to make an "as-pattern", simultaneously naming the tail of the list `xs` (for use if we're unlucky) and matching it as `(y : _)` (so we can see if we've won).

When I was a child, my father and I would have written something like this

    elementBefore elt (_ ++ x : elt : _) = Just x
    elementBefore _ _ = Nothing

but that has always been too simple to be valid Haskell.
