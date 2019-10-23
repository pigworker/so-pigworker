Not to gainsay `all isRight` as a good answer to the question which was asked, I'd question the question, to some extent. What good is it to compute, as a `Bool`, whether all the `Either` values in a list are `Right`? What does it enable you to do? One answer is that it entitles you to strip the `Right` tags from the entire list, treating the whole as error free.

A more informative option might be to construct something of type

    [Either String Int] -> Either String [Int]

so that instead of a mere `True` or `False`, you obtain all the `Int`s untagged or the message associated with the first pesky `Left`.

And there is a standard function which does this (and many other things besides). It exploits the fact that lists are a data structure with a standard traversal pattern and that `Either String` encodes a notion of error-managing computation with standard patterns of failure and success propagation. The type has already done the hard work. All you need to say is...

    sequenceA
