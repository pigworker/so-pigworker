The error you report is not the only error.

Let's put on the special spectacles which show the things usually kept invisible by "type inference".

Firstly, the data constructor:

    Simple :: forall a. (Typeable a, Show a) =>
              Message -> (String -> a) -> Question

Effectively, a value of type `Question` looks like

    Simple {a}{typeableDict4a}{showDict4a} message parser

where I've written the invisible things in braces. The constructor packs up a type and the two typeclass dictionaries that give the implementations for the members of `Typeable` and `Show`.

Now let's have the main program. I've renamed the type variable to make a point.

    runQuestion :: forall b. (Typeable b, Show b) => Question -> IO b

The type to be given back is chosen by the caller of `runQuestion`, separately from whatever type is packed inside the argument of type `Question`. Now let's fill in the invisible components in the program itself.

    runQuestion {b}{typeableDict4b}{showDict4b}
      (Simple {a}{typeableDict4a}{showDict4a} message parser) = do
                            -- so parser :: String -> a
          putStrLn message  -- ok, as message :: String
          ans <- getLine    -- ensures ans :: String
      return $ parser ans   -- has type IO a, not IO b

The `parser` computes a value of the type `a` packed up in the `Question`, which is totally separate from the type `b` passed directly to `runQuestion`. The program does not typecheck because there's a conflict between two types which can be made different by the program's caller.

Meanwhile, let's look at `print`

    print :: forall c. Show c => c -> IO ()

When you write

    main = getLine >>= (runQuestion . getQuestion) >>= print

you get

    main = getLine >>=
      (runQuestion {b}{typeableDict4b}{showDict4b} . getQuestion) >>=
      print {b}{showDict4b}

and as the return type of `runQuestion {b}` is `IO b`, it must be the case that `print`'s `c` type is the same as `runQuestion`'s `b` type, but other than that, there is *nothing* to determine which type `b` is, or why it is an instance either of `Typeable` or `Show`. With the type annotation, the need for `Typeable` shows up first (in the `runQuestion` call); without, the need for `Show` in `print` causes the complaint.

The real problem, is that somehow, you seem to want `runQuestion` to deliver a thing in whatever type is hidden inside the question, as if you could somehow write a (dependently typed) program like

    typeFrom :: Question -> *
    typeFrom (Simple {a}{typeableDict4a}{showDict4a} message parser) = a

    runQuestion :: (q :: Question) -> IO (typeFrom q)

That's a perfectly sensible thing to want, but it isn't Haskell: there's no way to name "the type packed up inside that argument". Everything which involves that type has to live in the scope of the case analysis or pattern match which exposes it. It's your attempt to do the `print` outside that scope that won't be allowed.
