The meaning of the `forever` in your code is that it takes for ever to choose *each* value in the stream. Its type

    forever :: Monad m => m a -> m b

is a big clue that a computation built with `forever` never returns a value: the caller of `forever` gets to pick the type `b` arbitrarily, so no program can actually promise to deliver a value of that type. That's also why your program typechecks. The computation you pass to `forever` is repeatedly executed for its effect (in this case, choosing a random number), but no value is ever delivered, hence the stream never gets going.

You shouldn't need a `forever` to make a stream that keeps going. The behaviour of `makeInputStream` is to run its argument computation each time a value is demanded from the stream, so you've got your repetition there already.
