Biased am I, but I think this is a great opportunity to make use of [Control.Newtype][1], a little piece of kit that's a mere "cabal install newtype" away.

Here's the deal. You want to flip around type constructors to get your hands on functoriality (for example) in a different parameter. Define a newtype

     newtype Flip f x y = Flip (f y x)

and add it to the `Newtype` class thus

     instance Newtype (Flip f x y) (f y x) where
       pack = Flip
       unpack (Flip z) = z

The `Newtype` class is just a directory mapping newtypes to their unvarnished equivalents, providing  handy kit, e.g. `op Flip` is the inverse of `Flip`: you don't need to remember what you called it.

For the problem in question, we can now do stuff like this:

     data Bif x y = BNil | BCons x y (Bif x y) deriving Show

That's a two parameter datatype which happens to be functorial in both parameters. (Probably, we should make it an instance of a Bifunctor class, but anyway...) We can make it a `Functor` twice over: once for the last parameter...

    instance Functor (Bif x) where
      fmap f BNil = BNil
      fmap f (BCons x y b) = BCons x (f y) (fmap f b)

...and once for the first:

    instance Functor (Flip Bif y) where
      fmap f (Flip BNil) = Flip BNil
      fmap f (Flip (BCons x y b)) = Flip (BCons (f x) y (under Flip (fmap f) b))

where `under p f` is a neat way to say `op p . f . p`.

I tell you no lies: let us try.

    someBif :: Bif Int Char
    someBif = BCons 1 'a' (BCons 2 'b' (BCons 3 'c' BNil))

and then we get

    *Flip> fmap succ someBif
    BCons 1 'b' (BCons 2 'c' (BCons 3 'd' BNil))
    *Flip> under Flip (fmap succ) someBif
    BCons 2 'a' (BCons 3 'b' (BCons 4 'c' BNil))

In these circumstances, there really are many ways the same thing can be seen as a `Functor`, so it's right that we have to make some noise to say which way we mean. But the noise isn't all that much if you're systematic about it.

  [1]: http://hackage.haskell.org/packages/archive/newtype/0.2/doc/html/Control-Newtype.html
