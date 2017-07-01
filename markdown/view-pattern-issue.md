They're not equivalent. The `case` version has one `readMaybe`, the view pattern version has two. For every `readMaybe`, the compiler has to infer which type is the target of the attempt to read. When the code says

    parse xs x = case readMaybe x of
      Just x  -> Right (x : xs)
      Nothing -> Left "Syntax error

the GHC detective notices that in your `Just x` case, `x` ends up consed to `xs`, and so must take whatever type the elements of `xs` have. And that's good work.

But when you write

    parse xs (readMaybe -> Just x ) = Right (x : xs)
    parse xs (readMaybe -> Nothing) = Left "Syntax error"

you create two separate find-the-target-type problems, one for each use of `readMaybe`. The first of these is solved in just the same way as in the `case` case, but for the second, read individually,

    parse xs (readMaybe -> Nothing) = Left "Syntax error"

there is just no clue *what* it is that you are failing to read, and no reason to believe it is the same thing as in the line above.

Generally, it is inappropriate to use view patterns unless there is only one outcome of interest. They are the wrong syntax if you want to do an intermediate computation *once*, but analyse the result into more than one case. I am happy to remain on the record that I consider them a misfeature for this reason.
