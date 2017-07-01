`()` means "Boring". It means the boring type which contains one thing, also boring. There is nothing interesting to be gained by comparing one element of the boring type with another, because there is nothing to learn about an element of the boring type by giving it any of your attention.

It is very different from the empty type, called (by people I wish had chosen a better name like the one I suggested) in Haskell `Void`. The empty type is very exciting, because if somebody ever gives you a value belonging to it, you know that you are already dead and in Heaven and that anything you want is yours.

But if somebody gives you a value in `()`, don't get excited. Just throw it away.

Sometimes it's fun to take type constructors parametrised by an "element type" and fill the parameter in with `()`. You can see just what information is inherent in the type constructor, rather than coming from the elements. E.g, `Maybe ()` is a version of `Bool`, with `Just ()` or `Nothing`. Also, `[()]` amounts to the (possibly infinite) natural numbers: the only information you have is a length.

So, `()` means "Boring", but it's often a clue that something interesting is happening somewhere else.
