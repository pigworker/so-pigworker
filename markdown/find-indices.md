I'd use `zip` and a list comprehension.

    indicesOf :: Eq a => a -> [a] -> [Int]
    indicesOf a as = [i | (b, i) <- zip as [0..], b == a]

Zipping with `[0..]` is a standard way to label every element with an index, and then it's a simple query.
