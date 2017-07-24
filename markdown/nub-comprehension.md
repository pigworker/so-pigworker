The comment from @amalloy that list comprehensions are confined to a "local" perspective is the key insight here. There is a sensible way to write `nub` as a list comprehension, but you first need to change your perspective.

An often useful function sadly omitted from the library is the function which decorates each element of a list with its context.

    picks :: [x] -> [([x], x, [x])]
    picks []       = []
    picks (x : xs) = ([], x, xs) : [(x : bs, y, as) | (bs, y, as) <- picks xs]

So

    picks [1,2,3] =
    [([],1,[2,3]), ([1],2,[3]), ([1,2],3,[])]

Each element of the list is put in the middle of a triple, with the elements 'before' to its left and the elements 'after' to its right.

[This answer of mine][1] explains the deep structure which makes `picks` in some sense a "standard" operation, derivable from the structure of lists. But we don't need that background information to deploy it.

The `picks` function gives us exactly the contextual information we need to write `nub` as a list comprehension. All we need to do is pick out the elements which don't occur in their own 'before lists'.

    myNub :: Eq x => [x] -> [x]
    myNub xs = [x | (bs, x, as) <- picks xs, not (elem x bs)]

I make no promises as to the efficiency of this operation, but I do like the clarity that comes from combining list comprehensions with extra spatial context.

  [1]: https://stackoverflow.com/a/12872133/828361
