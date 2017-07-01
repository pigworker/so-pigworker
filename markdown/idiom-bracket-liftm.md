`liftM` and friends serve the purpose of jacking up *pure* functions to a monadic setting, much as the `Applicative` combinators do.

    liftM :: Monad m => (s -> t) -> m s -> m t

There are two issues with the code you tried. One is just a lack of parentheses.

    liftM sendAllTo :: IO Socket -> IO (ByteString -> SockAddr -> IO ())

which is not what you meant. The other issue is that

    sendAllTo :: Socket -> ByteString -> SockAddr -> IO ()

is a *monadic* operation, so lifting it will deliver two layers of `IO`. The usual method is to parenthesize the pure prefix of the application, like so

    liftM (sendAllTo s datastring) :: IO SockAddr -> IO (IO ())

You can then build the argument with `liftM2`.

    liftM2 SockAddrInet ioport (inet_adder host) :: IO SockAddr

That gives you

    liftM (sendAllTo s datastring) (liftM2 SockAddrInet ioport (inet_adder host))
      :: IO (IO ())

which will achieve precisely nothing as it stands, because it explains how to compute an operation but doesn't actually invoke it! That's where you need

    join (liftM (sendAllTo s datastring) (liftM2 SockAddrInet ioport (inet_addr host)))
      :: IO ()

or, more compactly

    sendAllTo s datastring =<< liftM2 SockAddrInet ioport (inet_adder host)

**Plug.** The [Strathclyde Haskell Enhancement][1] supports [idiom brackets][2], where

`(|f a1 .. an|) :: m t` if `f :: s1 -> ... -> sn -> t` and `a1 :: m s1` ... `an :: m sn`.

These do the same job for `Applicative m` as the `liftM` family do for monads, treating `f` as a pure n-ary function and `a1`..`an` as effectful arguments. `Monad`s can and should be `Applicative` too, so

    (|SockAddrInet ioprot (inet_addr host)|) :: IO SockAddr

and

    (|(sendAllTo s datastring) (|SockAddrInet ioprot (inet_addr host)|)|) :: IO (IO ())

The notation then allows you to invoke computed monadic computations like the above, with a postfixed `@`.

    (|(sendAllTo s datastring) (|SockAddrInet ioprot (inet_addr host)|) @|) :: IO ()

Note that I'm still parenthesizing the pure prefix of the application, so that the `f` of the template is the whole of `(sendAllTo s datastring)`. The notation allows you to mark pure arguments in any position with a `~`, so you can write this

    (|sendAllTo ~s ~datastring (|SockAddrInet ioprot (inet_addr host)|) @|) :: IO ()

if the mood takes you.

**Rant.** We spend far too much energy on figuring out the right `liftM`, `join`, `=<<`, `do`, `(|...~...@|)` punctuation in order to explain how to cut up a type as a value-explaining kernel (`()` here) in an effect-explaining context (`IO` here). If this up-cutting were made more explicitly in types, we should need less noise in our programs to slap values and computations into alignment. I should much prefer the computer to infer where the `~` and `@` marks go, but as things stand, Haskell types are too ambiguous a document to make that feasible.

  [1]: https://personal.cis.strath.ac.uk/conor.mcbride/pub/she/
  [2]: https://personal.cis.strath.ac.uk/conor.mcbride/pub/she/idiom.html
