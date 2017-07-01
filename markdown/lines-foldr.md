Suppose you had already knew the result of `lines` for all but the first character of your input. How would you add the first character onto that result?

    charon :: Char -> [[Char]] -> [[Char]]
    charon '\n'     css  = [] : css    -- to begin with newline, insert blank line
    charon c         []  = [[c]]       -- very last char is in line of its own
    charon c (cs : css)  = (c : cs) : css  -- else put char in first line

And with that mystery solved,

    lines = foldr charon []

For years, I have made students bang their fists on the furniture and chant "*what* do you *do* with the *emp*ty *list*? *what* do you *do* with *x* cons *xs*?". Sometimes it helps.
