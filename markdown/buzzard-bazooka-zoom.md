The specification is not entirely clear, but it sounds like you want to collect all the characters which occur three places after a `'Z'` in the input, so that from

    "BUZZARD BAZOOKA ZOOM"

we get

    "RDKM"

Without a clearer presentation of the problem, it is difficult to give precise advice. But I hope I can help you get past some of the small irritations, so that you can engage with the actual logic of the problem.

Let's start with the type. You have

    someFun :: String => String -> String -> String

but left of `=>` is the place for *properties* of type expressions, usually involving variables that could stand for lots of types, such as `Eq a` (meaning that whatever type `a` is, we can test equality). `String` is a type, not a property, so it cannot stand left of `=>`. Drop it. That gives

    someFun  :: String -- input
             -> String -- accumulating the output (?)
             -> String -- output

It is not clear whether you really need an accumulator. Suppose you know the output for

    "ZARD BAZOOKA BOOM"  -- "DKM", right?

Can you compute the output for

    "ZZARD BAZOOKA BOOM"  -- "RDKM"

? Just an extra `'R'` on the front, right? You're using tail recursion to *do* the next thing, when it is usually simpler to think about what things should *be*. If you know what the output *is* for the tail of the list, then say what the output *is* for the whole of the list. Why not just map input to output directly, so

    someFun :: String -> String

Now, pattern matching, start with the simplest possible pattern

    someFun s = undefined

Can you see enough about the input to determine the output? Clearly not. It matters whether the input is empty or has a first character. Split into two cases.

    someFun ""      = undefined
    someFun (c : s) = undefined   -- c is the first Char, s is the rest of the String

It also matters whether the first character is `'Z'` or not. Be careful to use *single* quotes for `Char` and *double* quotes for `String`: they are different types.

    someFun ""         = undefined
    someFun ('Z' : s)  = undefined   -- the first Char is Z
    someFun (c : s)    = undefined

In the case wit `'Z'`, you also want to make sure that `s` has at least three characters, and we care about the third, so

    someFun ""                         = undefined   -- input empty
    someFun ('Z' : s@(_ : _ : d : _))  = undefined   -- first is 'Z' and d is 3 later
    someFun (c : s)                    = undefined   -- input nonempty

The `@` is an "as pattern", allowing me to name the whole tail `s` and also check that it matches `(_ : _ : d : _)`, grabbing the third character after the `'Z'`.

So far, I've given no thought to the output, just what I need to see about the input. Let's figure out what the output must be. In the first case, empty input gives empty output

    someFun ""                         = ""
    someFun ('Z' : s@(_ : _ : d : _))  = undefined   -- first is 'Z' and d is 3 later
    someFun (c : s)                    = undefined   -- input nonempty

and in the other two cases, we can assume that `someFun s` already tells us the output for the tail of the list, so we just need to figure out how to finish the output for the whole list. In the last line, the output for the tail is just what we want.

    someFun ""                         = ""
    someFun ('Z' : s@(_ : _ : d : _))  = undefined   -- first is 'Z' and d is 3 later
    someFun (c : s)                    = someFun s

But in the case where we've found that `d` is three places after the initial `'Z'`, we need to make sure `d` is at the start of the output.

    someFun ""                         = ""
    someFun ('Z' : s@(_ : _ : d : _))  = d : someFun s
    someFun (c : s)                    = someFun s

Just checking:

    *Main> someFun "BUZZARD BAZOOKA ZOOM"
    "RDKM"

The key idea is to figure out how to express the output for the whole input in terms of the output for its pieces: what *it is*, not what *to do*. Here, you can assume that the output for the tail, `s` is correctly computed, so you just need to figure out whether you have anything extra to return.
