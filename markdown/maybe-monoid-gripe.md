In `Data.Monoid`, a `newtype` copy of `Maybe`, called `First`, has the "take the first `Just`" behaviour.

If you were looking for a function of type

    [a -> First b] -> a -> First b

with the behaviour you describe, it would simply be

    fold

from `Data.Foldable`, because the monoid behaviour for `a ->` does the pointwise lifting needed: the `Monoid` for `a -> First b` is exactly picking the first application outcome whic works. Sadly (for my tears over this have been many), to get `Maybe` instead of `First` takes a little more work.

Note that the pointwise lifting, yanking `a ->` out through `[]`, is just the sort of job for `sequenceA`, so

    (asum .) . sequenceA

will do the job.

It's good to get the monoid structure you need cued from the type: in this case, accessing the `Alternative` behaviour with `asum` will have to do.
