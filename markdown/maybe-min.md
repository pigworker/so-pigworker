It is possible to satisfy the specification using operators from `Control.Applicative`.

    myMin :: Ord x => Maybe x -> Maybe x -> Maybe x
    myMin a b = min <$> a <*> b <|> a <|> b

where the `<|>` for `Maybe` implements "preference"

    Nothing <|> b  = b
    a       <|> _  = a

The thing is

    min <$> Just a <*> Just b = Just (min a b)

but

    min <$> Just a <*> Nothing = Nothing

which has resulted in some incorrect answers to this question. Using `<|>` allows you to prefer the computed `min` value when it's available, but recover with either individual when only one is `Just`.

**But** you should ask if it is appropriate to use `Maybe` in this way. With the inglorious exception of its `Monoid` instance, `Maybe` is set up to model failure-prone computations. What you have here is the extension of an existing `Ord` with a "top" element.

    data Topped x = Val x | Top deriving (Show, Eq, Ord)

and you'll find that `min` for `Topped x` is just what you need. It's good to think of types as not just the representation of data but the equipment of data with structure. `Nothing` usually represents some kind of failure, so it might be better to use a different type for your purpose.
