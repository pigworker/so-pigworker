Parametricity says no. The only functions of type

    a -> Bool

are constant functions.

However, with a bit of ad hoc polymorphism and a bit more chutzpah, you can do this:

    {-# LANGUAGE OverlappingInstances, FlexibleInstances #-}

    class Sick x where
      isFunc :: x -> Bool

    instance Sick (a -> b) where
      isFunc _ = True

    instance Sick x where
      isFunc _ = False

and then it looks like you have

    *Sick> isFunc 3
    False
    *Sick> isFunc id
    True

But it does seem like a peculiar thing to do. What use is the resulting `Bool` to you?
