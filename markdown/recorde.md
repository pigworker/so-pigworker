It would be unfortunate to write

    (0, _) = []

because that is not true.

In the tradition of Robert Recorde, we try to write equations only when we intend the left-hand side to equal the right-hand side. So we write

    dup x = (x, x)

to make `dup x` equal to `(x, x)`, or

    dup = \ x -> (x, x)

to make `dup` equal to the function which maps `x` to `(x, x)`, but not

    \ x = (x, x)

because there is no way to make `x` equal `(x, x)`.

We depart from the tradition only slightly when we allow "falling through", e.g.,

    f 0 = 1
    f n = 2 * f (n - 1)

but only in the sense that the second line has a silent "otherwise".
