Lists form a *monoid* structure, with associative binary operation `++` and neutral element `[]`. That is, we have

    [] ++ xs = xs = xs ++ []    (xs ++ ys) ++ zs = xs ++ (ys ++ zs)

Meanwhile, numbers have lots of monoid structure, but the relevant one here is that where the operation is `*` and the neutral element is `1`.

    1 * x = x = x * 1           (x * y) * z = x * (y * z)

The `product` function is not only a map from lists of numbers to numbers: it's a *monoid homomorphism*, reflecting the list monoid structure in the numerical monoid. Crucially,

    product (xs ++ ys) = product xs * product ys

and

    product [] = 1

In fact, to get the former, we pretty much have the latter forced upon us.
