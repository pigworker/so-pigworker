All these operators are left associative; the `<` and/or `>` points to things which contribute values; it's `$` for thing-to-left-is-pure-value and `*` for thing-to-left-is-applicative-computation.

My rule of thumb for using these operators goes as follows. First, list the components of your grammatical production and classify them as "signal" or "noise" depending on whether they contribute semantically important information. Here, we have

    char '('      -- noise
    buildExpr     -- signal
    char ')'      -- noise

Next, figure out what the "semantic function" is, which takes the values of the signal components and gives the value for the whole production.  Here, we have

    id     -- pure semantic function, then a bunch of component parsers
           char '('      -- noise
           buildExpr     -- signal
           char ')'      -- noise

Now, each component parser will need to be attached to what comes before it with an operator, but which?

  * always start with `<`
  * next `$` for the first component (as the pure function's just before), or `*` for every other component
  * then comes `>` if the component is *signal* or ` ` if it's *noise*

So that gives us

    id     -- pure semantic function, then a bunch of parsers
       <$  char '('      -- first, noise
       <*> buildExpr     -- later, signal
       <*  char ')'      -- later, noise

If the semantic function is `id`, as here, you can get rid of it and use `*>` to glue noise to the front of the signal which is `id`'s argument. I usually choose not to do that, just so that I can see the semantic function sitting clearly at the beginning of the production. Also, you can build a choice between such productions by interspersing `<|>` and you don't need to wrap any of them in parentheses.
