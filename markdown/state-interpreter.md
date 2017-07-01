It's a little difficult to answer your question, because you didn't actually ask one. Let me just pick out a few of the things that you've said, in order to give you a few clues.

> I am not sure if I need to use `evalE` in this problem or not. I have written it in a previous problem. The signature for `evalE` is `evalE :: Expression -> Store -> (Value, Store)`

> `evalS (Expr e) s         = ... Not sure what to do, here.`

What does it mean to execute a statement which consists of an expression? If it has something to do with evaluating the expression, then the fact that you have an expression evaluator available might help with "what to do, here".

Next, compare the code you've been given for "while" (which contains a fine example of a sensible thing to do with an expression, by the way)...

    evalS w@(While e s1) s = case (evalE e s) of`
      (BoolVal True,s')  -> let s'' = evalS s1 s' in evalS w s''
      (BoolVal False,s') -> s'
      _                  -> error "Condition must be a BoolVal"

...and compare it with your code for "if"

    evalS (If e s1 s2) s     = do
       x <- evalE e
       case x of
          BoolVal True -> evalS s1
          BoolVal False -> evalS s2

Your code is in a rather different style &mdash; the "monadic" style. Where are you getting that from? It would make sense if the types of the evaluators were something like

    evalE :: Expression -> State Store Value
    evalS :: Statement  -> State Store ()

The monadic style is a very nice way to thread the mutating store through the evaluation process without talking about it too much. E.g., your `x <- evalE e` means "let `x` be the result of evaluating `e` (quietly receiving the initial store and passing along the resulting store)". It's a good way to work which I expect you'll explore in due course.

But those aren't the types you've been given, and the monadic style is not appropriate. You have

    evalE :: Expression -> Store -> (Value, Store)
    evalS :: Statement  -> Store ->         Store

and the example code threads the store explicitly. Look again

    evalS w@(While e s1) s = case (evalE e s) of`
      (BoolVal True,s')  -> let s'' = evalS s1 s' in evalS w s''
      (BoolVal False,s') -> s'
      _                  -> error "Condition must be a BoolVal"

See? `evalS` receives its initial store, `s`, explicitly, and uses it explicitly in `evalE e s`. The resulting new store is called `s'` in both `case` branches. If the loop is over, then `s'` is given back as the final store. Otherwise, `s'` is used as the store for one pass through the loop body, `s1`, and the store `s''` resulting from that is used for the next time around the loop, `w`.

Your code will need to be similarly explicit in the way it names and uses the store at each stage of evaluation. Let's walk through.

    evalS Skip s             = show s -- I am assuming that since Skip returns an empty String, I just need to return an empty String.

You assume incorrectly. The `evalS` function does not return a `String`, empty or otherwise: it returns a `Store`. Now, which `Store`? Your initial store is `s`: how will the store after "skip" relate to `s`?

    evalS (Sequence s1 s2) s = evalS s1 >> evalS s2 -- sequence1 then sequence2. I am not quite sure what to do with the s.

Again, that's a monadic approach which does not fit with this question. You need to thread the store, intially `s`, through the process of evaluating statements `s1` and `s2` in sequence. The "while" case has a good example of how to do something like that.

    evalS (Expr e) s         = ... Not sure what to do, here.

Again, the "while" example shows you one way to extract a value and an updated store by evaluating an expression. Food for thought, isn't it?

    evalS (If e s1 s2) s     = ...

Now "if" starts out by evaluating a condition, rather a lot like "while", no?

So, my advice amounts to this:

 * drop the monadic style code for now, but come back to it later when it's appropriate;
 * read the example implementation of "while" and understand how it treats expressions and sequences of statements, passing the store explicitly;
 * deploy the similar techniques to implement the other constructs.

The person who set the question has been kind enough to give you code which gives an example of everything you will need. Please reciprocate that kindness by comprehending and then taking the hint!
