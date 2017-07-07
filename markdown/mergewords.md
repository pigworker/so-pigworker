The puzzle is effectively to merge a *list* of words, a character at a time, into lines with trailing newline characters.

    mergeWords :: [String] -> String

We need to take a list like

    [ "hello"
    , "jim"
    , "nice"
    , "day"
    ]

and rearrange it into the lists of things at a given position

    [ "hjnd"
    , "eiia"
    , "lmcy"
    , "le"
    , "o"
    ]

That's what the library function `transpose` does.

And then we need to make a single string which treats those as lines, separated by newlines. Which is what `unlines` does.

So

    mergeWords = unlines . transpose

and we're done.
