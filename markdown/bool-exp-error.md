Your suspicion, "I think there is a problem in the data BExp declaration itself", is correct. What you've written doesn't mean what I suspect you hope it does. The error is at the far right end (as errors often are) so I had to scroll to find it. Using more vertical space, let's see.

    data BExp
      = Eq AExp AExp
      | Lt AExp AExp
      | Gt AExp AExp
      | ELt AExp AExp
      | EGt AExp AExp
      | And BExp BExp
      | Or BExp BExp
      | Bool

And it's the last that is the big problem. It's harder to spot because although you tell *us*, "I want the type to be :: BExp -> Bool", you do not tell the compiler. If you had done the decent thing and communicated your intention by writing an explicit type signature, the error report might have been more helpful. Your program begins

    evalBExp True = True

and that is enough to convince the typechecker that the intended type is

    evalBExp :: Bool -> Bool

because `True :: Bool`. When line 3 shows up with

    evalBExp (Eq a1 a2) = evalAExp (a1) == evalAExp (a2)

suddenly it wonders why you're giving `evalBExp` a `BExp` instead of a `Bool`.

Now, I suspect that you have the impression that your final clause in `BExp`

    | Bool

makes `True :: BExp` and `False :: BExp`, but that's not what it does at all. Instead, you will discover that you have a nullary data constructor `Bool :: BExp` whose name is coincident with the datatype `Bool` but lives in an entirely separate namespace. I believe your intention is to embed the values of `Bool` into `BExp` silently, but Haskell does not allow such subtle subtyping. To achieve the intended effect, you will need a constructor which explicitly packs up a `Bool`, so try

    data BExp
      = ...
      | BVal Bool

and

    evalBExp :: BExp -> Bool
    evalBExp (BVal b) = b
    ...

to get closer to your plan.

You are, of course, free to use the name `Bool` for the `BVal` constructor, and so write

    data BExp
      = ...
      | Bool Bool

where the first `Bool` is a data constructor and the second is a type constructor, but I would find that choice confusing.
