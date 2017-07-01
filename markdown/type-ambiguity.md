This is a classic problem. The "ad hoc" polymorphism of type classes makes type inference incomplete, and you've just been bitten. Let's look at the pieces.

    read    :: Read x => String -> x
    flatten :: NestedList a -> [a]
    print   :: Show y => y -> IO ()

and we'll also have machine-generated instances for

    Read a => Read (NestedList a)
    Show a => Show (NestedList a)
    Read a => Read [a]
    Show a => Show [a]

Now let's solve the equations we get when we try to build the composition.

    print   .     flatten              .   read

          y = [a]         NestedList a = x

That means we need

    Show [a]                     Read (NestedList a)  

and thus

    Show a                       Read a

and we've used all our information without determining `a`, and hence the relevant `Read` and `Show` instances.

As J. Abrahamson has already suggested, you need to do something which determines the `a`. There are lots of ways to do it. I tend to prefer type annotations to writing strange terms whose only purpose is to make a type more obvious. I second the proposal to give a type to one of the components in the composition, but I'd probably pick `(read :: String -> NestedList Int)`, as that's the operation which introduces the ambiguously typed thing.
