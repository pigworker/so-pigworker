*This answer is in two parts. The first part addresses the question directly. The second part goes off on a tangent (literally) in order to dig about in the mathematics behind the first part: it may thus prove to be difficult material of limited interest, but I thought a few extremists might enjoy it.*

The answers I've seen so far make neat use of list comprehensions or their monadic equivalent, but they use *equality* to rule out duplicates, thus requiring an extra `Eq` constraint. Here's a solution which makes all the pairs of elements in two different *positions*.

Firstly, I write a handy function which decorates each element of a list with the list of elements in other positions: all the ways to "pick one and leave the others". It's very useful whenever lists are being used to collect stuff for selection-without-replacement, and it's something I find I use a lot.

    picks :: [x] -> [(x, [x])]
    picks [] = []
    picks (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- picks xs]

Note that `map fst . picks = id`, so that the selected element in each position of the result is the element from that position in the original list: that's what I meant by "decorates".

Now it's easy to pick two, using the same list comprehension method as in the other answers. But instead of selecting the first component from the list itself, we can select from its `picks`, at the same time acquiring the list of candidates for the second component.

    allPairs :: [x] -> [(x, x)]
    allPairs xs = [(y, z) | (y, ys) <- picks xs, z <- ys]

It's just as easy to get hold of the triples, taking `picks` twice.

    allTriples :: [x] -> [(x, x, x)]
    allTriples ws = [(x, y, z) | (x, xs) <- picks ws, (y, ys) <- picks xs, z <- ys]

For uniformity, it's almost tempting to make the code slightly less efficient, writing `(z, _) <- picks ys` rather than `z <- ys` in both.

If the input list has no duplicates, you won't get any duplicating tuples in the output, because the tuples take their elements from different positions. However, you will get

    Picks> allPairs ["cat", "cat"]
    [("cat","cat"),("cat","cat")]

To avoid that, feel free to use `allPairs . nub`, which removes duplicates before selection and demands once more an `Eq` instance for the element type.
____
***For extremists only: containers, calculus, comonads and combinatorics ahoy!***

`picks` is one instance of a more general construct, arising from the differential calculus. It's an amusing fact that for any given containery sort of a functor `f`, its mathematical derivative, ∂f, represents `f`-structures with one element removed. For example,

    newtype Trio x = Trio (x, x, x)   -- x^3

has derivative

    data DTrio x = Left3 ((), x, x) | Mid3 (x, (), x) | Right3 (x, x, ())  -- 3*x^2

A number of operations can be associated with this construction. Imagine we can really use ∂ (and we can kinda code it up using type families). We could then say

    data InContext f x = (:-) {selected :: x, context :: ∂f x}

to give a type of selected elements decorated by context. We should certainly expect to have the operation

    plug :: InContext f x -> f x   -- putting the element back in its place

This `plug` operation moves us towards the root if we're zippering about in a tree whose nodes are seen as containers of subtrees.

We should also expect `InContext f` to be a comonad, with

    counit :: InContext f x -> x
    counit = selected

projecting out the selected element and

    cojoin :: InContext f x -> InContext f (InContext f x)

decorating every element with its context, showing all possible way you could *refocus*, selecting a different element.

The inestimable Peter Hancock once suggested to me that we should also expect to be able to move "down" (meaning "away from the root"), collecting all the possible ways to pick an element-in-context from a whole structure.

    picks :: f x -> f (InContext f x)

should decorate every `x`-element in the input `f`-structure with its context. We should expect that

    fmap selected . picks = id

which is the law we had earlier, but also

    fmap plug (picks fx) = fmap (const fx) fx

telling us that every decorated element is a decomposition of the original data. We didn't have that law above. We had

    picks :: [x] -> [(x, [x])]

decorating every element not quite with something a bit like its context: from just the list of other elements, you can't see where the "hole" is. In truth,

    ∂[] x = ([x], [x])

separating the list of elements before the hole from the elements after the hole. Arguably, I should have written

    picks :: [x] -> [(x, ([x], [x]))]
    picks [] = []
    picks (x : xs) = (x, ([], xs)) : [(y, (x : ys, ys')) | (y, (ys, ys')) <- picks xs]

and that's certainly a very useful operation too.

But what's really going on is quite sensible, and only a slight abuse. In the code I originally wrote, I'm locally taking `[]` to represent *finite bags* or *unordered lists*. Bags are lists without a notion of specific position, so if you select one element, its context is just the bag of the remaining elements. Indeed

    ∂Bag = Bag   -- really? why?

so the right notion of `picks` is indeed

    picks :: Bag x -> Bag (x, Bag x)

Represent `Bag` by `[]` and that's what we had. Moreover, for bags, `plug` is just `(:)` and, up to bag equality (i.e., permutation), the second law for `picks` *does* hold.

Another way of looking at bags is as a power series. A bag is a choice of tuples of any size, with all possible permutations (*n!* for size *n*) identified. So we can write it combinatorially as a big sum of powers quotiented by factorials, because you have to divide by x^n by n! to account for the fact that each of the n! orders in which you could have chosen the x's gives you the same bag.

     Bag x = 1 + x + x^2/2! + x^3/3! + ...

so

    ∂Bag x = 0 + 1 + x      + x^2/2! + ...

shifting the series sideways. Indeed, you may well have recognized the power series for `Bag` as being that for `exp` (or *e*^x), which is famous for being its own derivative.

So, phew! There you go. An operation naturally arising from the datatype interpretation of the exponential function, being its own derivative, is the handy piece of kit for solving problems based on selection-without-replacement.
