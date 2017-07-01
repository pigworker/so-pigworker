Not every type is serializable. How can you establish an isomorphism between `String -> String` and `String`? If you give me `Read` and `Show` instances for `String -> String`, I can find a function which isn't serialisable like this:

    evil :: String -> String
    evil s = map succ (read s s ++ " evil")

Suppose

    read (show evil) = evil

We get

    evil (show evil)
      = map succ (read (show evil) (show evil) ++ " evil")
      = map succ (evil (show evil) ++ " evil")
      = map succ (evil (show evil)) ++ "!fwjm"

so if `evil (show evil)` is defined, then it has a first character `c` satisfying `c = succ c`, which is impossible.

In general, functions can't be serialized. Sometimes, we write datatypes which pack up functions, so not every datatype is serializable either. E.g.,

    data Psychiatrist
      = Listen (String -> Psychiatrist)
      | Charge Int

Sometimes, even for these types, you might choose to provide partial implementations of `Read` (with some cases missing) and `Show` (e.g., with placeholders for or tabulations of functions), but there is no canonical way to choose them or reason why you would expect both.

As others have mentioned, serious serialization is the preserve of `Serialize`. I tend to use `Show` and `Read` for diagnostic purposes, especially trying things out in ghci. For that purpose, `Show` is by far more useful, because ghci has a *Haskell* parser to do the reading.
