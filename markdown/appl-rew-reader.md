Being an idle goodfornothing, I thought I would make a computer do the expansion for me. So into GHCi, I typed

    let pu x = "(\\_ -> " ++ x ++ ")"
    let f >*< a = "(\\g -> " ++ f ++ " g (" ++ a ++ " g))"

So now I have funny versions of `pure` and `<*>` which map strings which look like expressions to string which look like more complicated expressions. I then defined, similarly, the analogue of `sequenceA`, replacing functions by strings.

    let sqa [] = pu "[]" ; sqa (f : fs) = (pu "(:)" >*< f) >*< sqa fs

I was then able to generate the expanded form of the example as follows

    putStrLn $ sqa ["(+3)","(+2)"] ++ " 3"

which duly printed

    (\g -> (\g -> (\_ -> (:)) g ((+3) g)) g ((\g -> (\g -> (\_ -> (:)) g ((+2) g)) g  ((\_ -> []) g)) g)) 3

This last, copied to the prompt, yielded

    [6,5]

Comparing the output from my "metaprogram" with the attempt in the question shows a shorter initial prefix of lambdas, arising from a shallower nesting of `<*>` operations. Remember, it's

    (pure (:) <*> (+3)) <*> ((pure (:) <*> (+2)) <*> pure [])

so the outer `(:)` should be only three lambdas deep. I suspect the proposed expansion may correspond to a differently bracketed version of the above, perhaps

    pure (:) <*> (+3) <*> pure (:) <*> (+2) <*> pure []

Indeed, when I evaluate

    putStrLn $ pu "(:)" >*< "(+3)" >*< pu "(:)" >*< "(+2)" >*< pu "[]" ++ " 3 "

I get

    (\g -> (\g -> (\g -> (\g -> (\_ -> (:)) g ((+3) g)) g ((\_ -> (:)) g)) g ((+2) g)) g ((\_ -> []) g)) 3

which looks like it matches the (updated)

    (\d -> (\c -> (\b -> (\a -> (\_ -> (:)) a ((+3) a)) b ((\_ -> (:)) b)) c ((+2) c)) d ((\_ -> []) d)) 3

I hope this machine-assisted investigation helps to clarify what's going on.
