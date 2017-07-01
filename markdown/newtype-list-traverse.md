I'd start by defining the traditional "indicator function"

    indicate :: Num a => Bool -> a
    indicate b = if b then 1 else 0

so that

    indicate . isVowel :: Char -> Integer

Next, I'd get hold of two key pieces of kit from `Control.Arrow`

    (&&&) :: (x -> y) -> (x -> z) -> x -> (y, z)
    (***) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)

so (remembering that some characters are neither vowels nor consonants)

    (indicate . isVowel) &&& (indicate . isConsonant)
      :: Char -> (Integer, Integer)

And then I'd grab hold of `Sum` from `Data.Monoid`.

    (Sum . indicate . isVowel) &&& (Sum . indicate . isConsonant)
      :: Char -> (Sum Integer, Sum Integer)
    getSum *** getSum :: (Sum Integer, Sum Integer) -> (Integer, Integer)

Now I deploy `foldMap`, because we're doing some sort of monoidal "crush".

    (getSum *** getSum) .
    foldMap ((Sum . indicate . isVowel) &&& (Sum . indicate . isConsonant))
      :: String -> (Integer, Integer)

Then I remember that I wrote some code which got turned into `Control.Newtype` and I discover the following is missing but should be there.

    instance (Newtype n o, Newtype n' o') => Newtype (n, n') (o, o') where
      pack = pack *** pack
      unpack = unpack *** unpack

And now I need only write

    ala' (Sum *** Sum) foldMap ((indicate . isVowel) &&& (indicate . isConsonant))
      :: String -> (Integer, Integer)

The key gadget is

    ala' :: (Newtype n o, Newtype n' o') =>
     (o -> n) -> ((a -> n) -> b -> n') -> (a -> o) -> b -> o'
    -- ^-packer    ^-higher-order operator  ^-action-on-elements

where the packer's job is to select the newtype with the correct behavioral instance and also determine the unpacker. It's exactly designed to support working locally at a more specific type that signals the intended structure.
