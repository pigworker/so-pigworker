When I was a wee boy back in the 1980s, I implemented a functional language with complex patterns in that style. It amounted to allowing ++ in patterns. The resulting patterns were ambiguous, so matching involved a backtracking search process: the programmer could effectively specify whether to minimize or maximize the length of the prefix matching the pattern left of ++. The language had a form of "pattern guards", so that a candidate match could be tested to see if a subsequent computation succeeded and rejected in the case of failure. The resulting programs were often in-your-face obvious as to their meaning. It was a lot of fun.

These days, when faced with such problems, I reach for `span`, and if that won't cut it, I use parser combinators.

    span :: (a -> Bool) -> [a] -> ([a], [a])

> span, applied to a predicate p and a list xs, returns a tuple where first element is longest prefix (possibly empty) of xs of elements that satisfy p and second element is the remainder of the list

So, in particular `span (/= ',')` will split a `String` into whatever is before the first comma (or the whole thing if there is no comma), and the rest (starting with the comma if there is one).

And if that won't cut it, I use parser combinators.

But I always remember how it used to be easy.
