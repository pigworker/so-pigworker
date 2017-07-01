Your point that `takeWhile` doesn't work because you have no contextual information for the individual elements suggests the following strategy: get it.

[This](https://stackoverflow.com/a/12872133/828361) answer of mine makes reference to the decorate-with-context operation, which I called `picks` (because it shows you all the way to pick one element on which to focus). It's the general decorate-with-its-context operation that we just ought to have for free for every containery thing. For lists, it is

    picks :: [x] -> [(x, ([x], [x]))] -- [(x-here, ([x-before], [x-after]))]
    picks [] = []
    picks (x : xs) = (x, ([], xs)) : [(y, (x : ys, zs)) | (y, (ys, zs)) <- picks xs]

and it works perfectly well for infinite lists, while we're about it.

Now have a go with

    takeUntilDuplicate :: Eq x => [x] -> [x]
    takeUntilDuplicate = map fst . takeWhile (\ (x, (ys, _)) -> not (elem x ys)) . picks

(Curiously, I'm disturbed that the above one-liner is rejected for ambiguity of `Eq` if not given the above type signature. I'm tempted to ask a question about it, here. *Oh, it's the monomorphism restriction. How annoying.*)

**Remark.** It makes a lot of sense to (and I normally would) represent the "elements before" component that `picks` delivers using snoc-lists (lists which grow on the right), the better to preserve sharing and visual left-to-right-ness.
