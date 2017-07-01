It might be worth paying attention to the key semantic difference between `Applicative` and `Monad`, in order to determine when each is appropriate. Compare types:

    (<*>) :: m (s -> t) -> m s -> m t
    (>>=) :: m s -> (s -> m t) -> m t

To deploy `<*>`, you choose two computations, one of a function, the other of an argument, then their values are combined by application. To deploy `>>=`, you choose one computation, and you explain how you will make use of its resulting values to choose the next computation. It is the difference between "batch mode" and "interactive" operation.

When it comes to parsing, `Applicative` (extended with failure and choice to give `Alternative`) captures the *context-free* aspects of your grammar. You will need the extra power that `Monad` gives you only if you need to inspect the parse tree from part of your input in order to decide what grammar you should use for another part of your input. E.g., you might read a format descriptor, then an input in that format. Minimizing your usage of the extra power of monads tells you which value-dependencies are essential.

Shifting from parsing to parallelism, this idea of using `>>=` only for essential value-dependency buys you clarity about opportunities to spread load. When two computations are combined with `<*>`, neither need wait for the other. Applicative-when-you-can-but-monadic-when-you-must is the formula for speed. The point of `ApplicativeDo` is to automate the dependency analysis of code which has been written in monadic style and thus accidentally oversequentialised.

Your question also relates to coding style, about which opinions are free to differ. But let me tell you a story. I came to Haskell from Standard ML, where I was used to writing programs in direct style even if they did naughty things like throw exceptions or mutate references. What was I doing in ML? Working on an implementation of an ultra-pure type theory (which may not be named, for legal reasons). When working *in* that type theory, I couldn't write direct-style programs which used exceptions, but I cooked up the applicative combinators as a way of getting as close to direct style as possible.

When I moved to Haskell, I was horrified to discover the extent to which people seemed to think that programming in pseudo-imperative do-notation was just punishment for the slightest semantic impurity (apart, of course, from non-termination). I adopted the applicative combinators as a style choice (and went even closer to direct style with "idiom brackets") long before I had a grasp of the semantic distinction, i.e., that they represented a useful weakening of the monad interface. I just didn't (and still don't) like the way do-notation requires fragmentation of expression structure and the gratuitous naming of things.

That's to say, the same things that make functional code more compact and readable than imperative code also make applicative style more compact and readable than do-notation. I appreciate that `ApplicativeDo` is a great way to make more applicative (and in some cases that means *faster*) programs that were written in monadic style that you haven't the time to refactor. But otherwise, I'd argue applicative-when-you-can-but-monadic-when-you-must is also the better way to see what's going on.