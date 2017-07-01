One of my favourite underappreciated utilities is rather handy for problems like this. Let me have backward lists, so I don't need to reverse my brain.

    data Bwd x = B0 | Bwd x :< x  -- rightmost is nearest

Think of list elements as the beads on an abacus wire. Flick a few to the left and leave your finger resting on the next one. What have you? A list of beads to the left of your finger (with the rightmost nearest), a list of beads to the right of your finger (with the leftmost nearest), and the bead with your finger on it.

That is, a *one-hole element context* for lists is given by the pair of backward and forward lists either side of the hole.

    type ListContext x = (Bwd x, [x])

Those who know my old songs recognize `ListContext` as the *derivative* of `[]`.

An element in focus (your finger on a bead) is

    type ListFocus x = (ListContext x, x)

And there is a useful operation which decorates every list element with its context, putting it in focus.

    focus :: [x] -> [ListFocus x]
    focus = go B0 where
      go xz [] = []
      go xz (x : xs) = ((xz, xs), x) : go (xz :< x) xs

For example,

    focus [1,2,3] = [((B0,[2,3]),1), ((B0 :< 1,[3]),2), ((B0 :< 1 :< 2,[]),3)]

and now it is very easy to answer all sorts of questions that concern an element and its immediate surroundings. You mightn't construct `focus` just to solve this problem, but it's the sort of thing I keep around because it solves lots of problems.

    [p | ((_ :< p,_),q) <- focus xs, q == x]

computes all the values `p` which sit to the left of an `x` in `xs`. *As you can see.*

(By the way, this `focus` operation didn't come from nowhere. It arises from the differential structure of the datatype of lists. [This answer][1] (where it is called `picks`) tells the list story in more detail, and [this answer][2] develops the datatype generic story.)


  [1]: https://stackoverflow.com/a/12872133/828361
  [2]: https://stackoverflow.com/a/25572148/828361
