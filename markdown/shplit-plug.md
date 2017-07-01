The datatype declaration for `Expr` gives rise, systematically, to a set of patterns which cover all possible things that a value of type `Expr` can *be*. Let's do the translation

    data Expr           -- any e :: Expr must be one of
      = T               -- T
      | Var Variable    -- (Var x)         -- where x :: Variable
      | And Expr Expr   -- (And e1 e2)     -- where e1 :: Expr, e2 :: Expr
      | Not Expr        -- (Not e1)        -- where e1 :: Expr

You can see that the `T`, `Var`, `And` and `Not` that head up each `data` clause are **constructors**, and live in the value language; the rest of the things in each clause live in the type language, saying what type each component of an `Expr` must have. Each of the corresponding **patterns** consists of the constructor applied to **pattern variables** standing for components which have the given types. Basically, the patterns that show up on the left-hand side of a function are made by repeatedly refining pattern variables to the patterns that their values can possibly take, *as indicated by their type*.

Writing a function by pattern matching does not consist of saying what to *do*: it consists of saying what the output *is* for the possible cases of what the input *is*. You need to analyse the input into cases where you can easily say what the output must be. So, start with one general case...

    v :: Expr -> [Variable]
    v e = undefined

...and refine it. Ask "Can you tell what it is yet?". We can't tell what `v e` is without knowing more about `e`. So we'd better *split* e. We know that `e :: Expr`, so we know what patterns its value can match. Make four copies of your program line, and in each, replace `e` by one of the four possible pattern listed above.

    v :: Expr -> [Variable]
    v T           = undefined
    v (Var x)     = undefined
    v (And e1 e2) = undefined
    v (Not e1)    = undefined

Now, in each case, can you tell what the output is? The handy thing is that you can make use of recursive calls on components. Assume you already know what `vars e1` and `vars e2` are when you're trying to say what `v (And e1 e2)` must be. If you get the steps right, the program will be correct.

I find it's often helpful to think in terms of concrete examples. Take your test example.

    v (Not (And (Var "y") T))

That's supposed to be `["y"]`, right? Which pattern does it match?

    Not e1   -- with e1 = And (Var "y") T

What's

    v e1

? Looking at it, it had better be

    ["y"]

In this example, what's `v (Not e1)` in terms of `v e1`? The very same. That might suggest a suitable expression to replace `undefined` in

    v (Not e1) = undefined  -- can you tell what this is now?

(Of course, a suggestive example is just a good start, not a guarantee of correctness.)

The takeaway messages: (1) build patterns by splitting pattern variables, figuring out the possible patterns by looking at the declaration of the type; (2) assume that recursive calls on components give you the right answer, then try to construct the right answer for the whole problem.

*Shameless plug: [shplit][1] is a simple tool I built for my students, capturing message (1) mechanically.*


  [1]: http://personal.cis.strath.ac.uk/~conor/pub/shplit
