Every Traversable functor is a container with finitely many positions for elements. In order to combine the effects for computations at each element, there must only be finitely many. So, for example, the infinite `Stream` functor is not Traversable, because there is no way to deliver a reliable function which pulls `Maybe` through. We'd need

    sequence :: Stream (Maybe x) -> Maybe (Stream x)

but if you want to check that everything in the stream succeeds, you'll have a long wait.

Zippers correspond to the ability to identify a particular element position (which further gives rise to a connection with derivatives, but that's another story). To be able to plug an element back in its hole, you need an effective way to decide equality on positions. If you have only finitely many positions, that's bound to be true (in the absence of information-hiding). So being Traversable is certainly sufficient for having a Zipper.

It's not necessary, however. `Stream` has a perfectly sensible Zipper

    type StreamContext x = ([x], Stream x)
    type StreamZipper x = (StreamContext x, x)

which represents the context as a finite (ok, ok, add a bang or two) list before the selected element and an infinite stream after it.

The positions in an infinite `Stream` are natural numbers. Natural numbers have a decidable equality, but there are infintely many of them.

tl;dr finite implies countable, but not vice versa.
