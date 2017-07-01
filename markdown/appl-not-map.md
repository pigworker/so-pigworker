All the suggestions so far are good. Here's another, which might seem a bit weird at first, but turns out to be quite handy in lots of other situations.

Some type-forming operators, like `[]`, which is the operator which maps a type of elements, e.g. `Int` to the type of lists of those elements, `[Int]`, have the property of being `Applicative`. For lists, that means there is some way, denoted by the operator, `<*>`, pronounced "apply", to turn *lists* of functions and *lists* of arguments into *lists* of results.

    (<*>) :: [s -> t] -> [s] -> [t]    -- one instance of the general type of <*>

rather than your ordinary application, given by a blank space, or a `$`

    ($)   :: (s -> t) ->  s  ->  t

The upshot is that we can do ordinary functional programming with lists of things instead of things: we sometimes call it "programming in the list *idiom*". The only other ingredient is that, to cope with the situation when some of our components are individual things, we need an extra gadget

    pure :: x -> [x]   -- again, one instance of the general scheme

which wraps a thing up as a list, to be compatible with `<*>`. That is `pure` moves an ordinary value into the applicative idiom.

For lists, `pure` just makes a singleton list and `<*>` produces the result of every pairwise application of one of the functions to one of the arguments. In particular

    pure f <*> [1..10] :: [Int -> Int -> Int -> Int -> Int]

is a list of functions (just like `map f [1..10]`) which can be used with `<*>` again. The rest of your arguments for `f` are not listy, so you need to `pure` them. 

    pure f <*> [1..10] <*> pure 1 <*> pure 2 <*> pure 3 <*> pure 4

For lists, this gives

    [f] <*> [1..10] <*> [1] <*> [2] <*> [3] <*> [4]

i.e. the list of ways to make an application from the f, one of the [1..10], the 1, the 2, the 3 and the 4.

The opening `pure f <*> s` is so common, it's abbreviated `f <$> s`, so

    f <$> [1..10] <*> [1] <*> [2] <*> [3] <*> [4]

is what would typically be written. If you can filter out the `<$>`, `pure` and `<*>` noise, it kind of looks like the application you had in mind. The extra punctuation is only necessary because Haskell can't tell the difference between a listy computation of a bunch of functions or arguments and a non-listy computation of what's intended as a single value but happens to be a list. At least, however, the components are in the order you started with, so you see more easily what's going on.

**Esoterica.** (1) in my (not very) [private dialect][1] of Haskell, the above would be

    (|f [1..10] (|1|) (|2|) (|3|) (|4|)|)

where each *idiom bracket*, `(|f a1 a2 ... an|)` represents the application of a pure function to zero or more arguments which live in the idiom. It's just a way to write

    pure f <*> a1 <*> a2 ... <*> an

Idris has idiom brackets, but Haskell hasn't added them. Yet.

(2) In languages with algebraic effects, the idiom of nondeterministic computation is not the same thing (to the typechecker) as the data type of lists, although you can easily convert between the two. The program becomes

    f (range 1 10) 2 3 4

where range nondeterministically chooses a value between the given lower and upper bounds. So, nondetermism is treated as a *local* side-effect, not a data structure, enabling operations for failure and choice. You can wrap nondeterministic computations in a *handler* which give meanings to those operations, and one such handler might generate the list of all solutions. That's to say, the extra notation to explain what's going on is pushed to the boundary, rather than peppered through the entire interior, like those `<*>` and `pure`. 

Managing the boundaries of things rather than their interiors is one of the few good ideas our species has managed to have. But at least we can have it over and over again. It's why we farm instead of hunting. It's why we prefer static type checking to dynamic tag checking. And so on...

  [1]: https://personal.cis.strath.ac.uk/conor.mcbride/pub/she/idiom.html
