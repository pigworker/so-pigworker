I'd write

    integer :: Parser Integer
    integer = read <$ many1 space <*> many1 digit

There's a bunch of left associative (like application) parser-building operators `<$>`, `<*>`, `<$`, `<*`. The thing in the far left should be the pure function which assembles the result value from the component values. The thing on the right of each operator should be a parser, collectively giving the components of the grammar left-to-right. Which operator to use depends on two choices, as follows.

      the thing to the right is    signal  / noise
      _________________________            
      the thing to the left is \           
                                +-------------------
                        pure /  |   <$>       <$
                      a parser  |   <*>       <*

So, having chosen `read :: String -> Integer` as the pure function which is going to deliver the semantics of the parser, we can classify the leading space as "noise" and the bunch of digits as "signal", hence

     read <$ many1 space <*> many1 digit
     (..)    (.........)     (.........)
     pure    noise parser     |
     (.................)      |
         parser              signal parser
     (.................................)
                        parser

You can combine multiple possibilities with

    p1 <|> ... <|> pn

and express impossibility with

    empty

It's seldom necessary to name components in parsers, and the resulting code looks more like a grammar with added semantics.
