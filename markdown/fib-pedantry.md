My, what a rat's nest of subtle terminological distinctions. What is "this"?

    fib=0:1:zipWith (+) fib (tail fib)

It is not a recursive function. It is not recursive data. It is a recursive definition.

What is being defined?

    fib

What type of thing is `fib`, according to this definition?

    [Integer]

A list of integers (or perhaps a list of any old numeric stuff).

Is `fib` a function? No, it is a list. Is `fib` recursively defined? Yes. Would `fib` be recursively defined if we replaced `zipWith` by a nonrecursive function of the same type (e.g. `\ f xs ys -> xs`)? Yes, although it would be a different recursively defined list.

Is `fib` a cyclic list? No. Does "recursive data structure" mean "cyclic data structure"? Not according to Hoare's paper, "Recursive Data Structures": http://portal.acm.org/book_gateway.cfm?id=63445&type=pdf&bookpath=%2F70000%2F63445%2Fcb-p217-hoare.pdf&coll=&dl=&CFID=15151515&CFTOKEN=6184618

In a typed setting, "recursive data structure" means no more or less than "inhabitant of a recursively defined type". Correspondingly `"fred"` is a recursive data structure, even though it is not recursively defined, and indeed it can be acted upon by recursive functions such as `++`.

The phrase "recursive function" means "recursively defined function". The phrase "recursive value" means "recursively defined value", such as exist in nonstrict languages: strict languages have the "value recursion" problem.

And if you think that's pedantic, try defining `fib` that way in a *total* programming language, and you'll discover that the notion of "recursive definition" splits into "definition by structural recursion" (consuming data in a way which stops) and "definition by guarded corecursion" (producing data in a way which goes), and that `fib` is of the latter variety. In that setting, the productivity of `fib` depends crucially on the laziness of `zipWith`. In the Haskell setting, of course, you don't need to worry about any of that stuff to figure out what sort of definition something is, just to figure out whether it has half a chance of actually working.
