The `foldl` and `foldr` functions are list-*consumers*. As [svenningsson's answer][1] rightly points out, `unfoldr` is a list-*producer* which is suitable for capturing the *co*-recursive structure of `fibs`.

However, given that `foldl` and `foldr` are polymorphic in their return types, i.e. what they're producing by consuming a list, it is reasonable to ask whether they might be used to consume one list and produce another. Might any of these produced lists be infinite?

Looking at the definition of `foldl`

    foldl :: (a -> b -> a) -> a -> [b] -> a
    foldl f a []        = a
    foldl f a (b : bs)  = foldl f (f a b) bs

we see that for `foldl` to produce anything at all, the list it consumes must be finite. Thus if `foldl f a` produces infinite output, it is because `a` is infinite or because `f` sometimes performs infinite list generation.

It's a different story with `foldr`

    foldr :: (b -> a -> a) -> a -> [b] -> a
    foldr f a []        = a
    foldr f a (b : bs)  = f b (foldr f a bs)

which admits the lazy possibility that `f` might generate some output for each `b` consumed from the input. Operations like

    map g = foldr (\ b gbs -> g b : gbs) []   -- golfers prefer ((:) . g)
    stutter = foldr (\ x xxs -> x : x : xxs) []

producing a little bit of output for each input, deliver infinite output from infinite input.

A cheeky person can thus express any infinitary recursion as a non-recursive `foldr` on an infinite list. E.g.,

    foldr (\ _ fibs -> 1 : 1 : zipWith (+) fibs (tail fibs)) undefined [1..]

(Edit: or, for that matter

    foldr (\_ fib a b -> a : fib b (a + b)) undefined [1..] 1 1

which is closer to the definition in the question.)

although this observation, whilst true, is hardly indicative of a healthy programming style.

  [1]: https://stackoverflow.com/a/12298382/828361
