A useful thing to try interactively in `ghci` is the `:info <something>` command, which can sometimes tell you helpful things.

    > :info Floating
    class Fractional a => Floating a where
      pi :: a
      exp :: a -> a
      log :: a -> a
      sqrt :: a -> a
      (**) :: a -> a -> a
      ---------------------------------------- loads more stuff
             -- Defined in ‘GHC.Float’
    instance Floating Float -- Defined in ‘GHC.Float’
    instance Floating Double -- Defined in ‘GHC.Float’

What does this mean? `Floating` is a *type class*. There is more than one type of floating point numbers. Indeed, two come as standard: `Float` and `Double`, where `Double` gives you twice the precision of `Float`. The `a` in `Floating a` stands for any type, and the big list of operations (including `sqrt`) is an interface which any instance of the class must implement. The fact that `sqrt` is in the interface for `Floating` means that it can always and only be used for instances of `Floating`. That is, to you its type is given as you say

    sqrt :: Floating a => a -> a

The `=>` syntax signals a *constraint*, here `Floating a` to its left. The type says

> for any type `a` which is an instance of `Floating`, given an input of type `a`, the output will have type `a`

You can specialize this type by filling in `a` with any type for which the constraint `Floating a` can be satisfied, so the following are both true

    sqrt :: Float -> Float
    sqrt :: Double -> Double

Now, `Float` and `Double` are represented by different bit-patterns, so the computational mechanisms for taking square roots is different in each case. It's handy not to have to remember different names for the different versions used for different types. The "constraint" `Floating a` really stands for the record (or *dictionary*) of the implementations for type `a` of all the operations in the interface. What the type of `sqrt` is really saying is

> given a type `a` and a dictionary of implementations for all the `Floating` operations, I'll take an `a` and give you an `a`

and it works by extracting the relevant `sqrt` implementation from the dictionary and using it on the given input.

So the `=>` signals a function type with an invisible dictionary input just as `->` signals a function type with a visible value input. You don't (indeed, you can't) write the dictionary when you use the function: the compiler figures it out from the type. When we write

    sqrt :: Double -> Double

we mean the general `sqrt` function invisibly applied to the `Floating Double` dictionary.
