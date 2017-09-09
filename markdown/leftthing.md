When you declare

    leftthing :: a -> Leftthing a

you are saying that the *caller* of `leftthing` gets to choose what `a` is.

When you then write

    leftthing (Twothings a b) = leftthing a

you are *presuming* that they have chosen a `Twothings` type, and as that is not necessarily the case, your program is rejected.

You may have thought that you were *testing whether* they had chosen a `Twothings` type, but no! Type information is erased before run time, so there is no way to make such a test.

You *can* try to restore the necessary run time information. First let me fix the inconsistency between your `Leftthing` and `leftthing`.

    type family Leftthing a where
      Leftthing (Twothings a b) = Leftthing{-you forgot the recursion!-} a
      Leftthing a = a

Now we can define the GADT of witnesses to `Twothing`ness.

    data IsItTwothings :: * -> * where
      YesItIs   :: IsItTwothings a -> IsItTwothings (Twothings a b)
      NoItIsn't :: Leftthing a ~ a => IsItTwothings a
                -- ^^^^^^^^^^^^^^^ this constraint will hold for any type
                -- which is *definitely not* a Twothings type

And then we can pass the witness as an argument:

    leftthing :: IsItTwothings a -> a -> Leftthing a
    leftthing (YesItIs r) (Twothings a b) = leftthing r a
    leftthing NoItIsn't   b               = b

In effect, the witness is the unary encoding of the number of left-nested `Twothings`es at the root of your type. That's enough information to determine at run time the correct amount of unpacking to do.

    > leftthing (YesItIs (YesItIs NoItIsn't)) (Twothings (Twothings True 11) (Twothings "strange" [42]))
    True

To sum up, you can't find out a type by pattern matching on a value. Rather, you need to know the type to do pattern matching (because the type determines the memory layout, and there are no run time type tags). You can't pattern match on types directly (because they're just not there to be matched on). You can construct data types which act as run time evidence of type structure and match on those instead.

Perhaps, one day, your program will work if you give it the type

    leftthing :: pi a. a -> Leftthing a

where `pi` is the dependent quantifier, indicating that the hidden type argument is not erased, but rather passed and matched on at run time. That day has not yet come, but I think it will.
