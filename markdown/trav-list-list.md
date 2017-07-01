Will this do?

    flip . (evalState .) . traverse . traverse . const . state $ head &&& tail

EDIT: let me expand on the construction...

The essential centre of it is `traverse . traverse`. If you stare at the problem with sufficiently poor spectacles, you can see that it's "do something with the elements of a container of containers". For that sort of thing, `traverse` (from `Data.Traversable`) is a very useful gadget (ok, I'm biased).

    traverse :: (Traversable f, Applicative a) => (s -> a t) -> f s -> a (f t)

or, if I change to longer but more suggestive type variables

    traverse :: (Traversable containerOf, Applicative doingSomethingToGet) =>
                  (s -> doingSomethingToGet t) ->
                  containerOf s -> doingSomethingToGet (containerOf t)

Crucially, `traverse` preserves the structure of the container it operates on, whatever that might be. If you view `traverse` as a higher-order function, you can see that it gives back an operator on containers whose type fits with the type of operators on elements it demands. That's to say `(traverse . traverse)` makes sense, and gives you structure-preserving operations on *two* layers of container.

    traverse . traverse ::
      (Traversable g, Traversable f, Applicative a) => (s -> a t) -> g (f s) -> a (g (f t))

So we've got the key gadget for structure-preserving "do something" operations on lists of lists. The `length` and `splitAt` approach works fine for lists (the structure of a list is given by its length), but the essential characteristic of lists which enables that approach is already pretty much bottled by the `Traversable` class.

Now we need to figure out how to "do something". We want to replace the old elements with new things drawn successively from a supply stream. If we were allowed the side-effect of updating the supply, we could say what to do at each element: "return `head` of supply, updating supply with its `tail`". The `State s` monad (in `Control.Monad.State` which is an instance of `Applicative`, from `Control.Applicative`) lets us capture that idea. The type `State s a` represents computations which deliver a value of type `a` whilst mutating a state of type `s`. Typical such computations are made by this gadget.

    state     :: (s -> (a, s)) -> State s a

That's to say, given an initial state, just compute the value and the new state. In our case, `s` is a stream, `head` gets the value, `tail` gets the new state. The `&&&` operator (from `Control.Arrow`) is a nice way to glue two functions on the same data to get a function making a pair. So

    head &&& tail :: [x] -> (x, [x])

which makes

    state $ head &&& tail :: State [x] x

and thus

    const . state $ head &&& tail :: u -> State [x] x

explains what to "do" with each element of the old container, namely ignore it and take a new element from the head of the supply stream.

Feeding that into `(traverse . traverse)` gives us a big mutatey traversal of type

    f (g u) -> State [x] (f (g x))

where `f` and `g` are any `Traversable` structures (e.g. lists).

Now, to extract the function we want, taking the initial supply stream, we need to unpack the state-mutating computation as a function from initial state to final value. That's what this does:

    evalState :: State s a -> s -> a

So we end up with something in

    f (g u) -> [x] -> f (g x)

which had better get flipped if it's to match the original spec.

**tl;dr** The `State [x]` monad is a readymade tool for describing computations which read and update an input stream. The `Traversable` class captures a readymade notion of structure-preserving operation on containers. The rest is plumbing (and/or golf).
