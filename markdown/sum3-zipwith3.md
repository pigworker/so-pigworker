I've seen this sort of question before, here: https://stackoverflow.com/q/21349408/828361 [My answer][1] to that question also pertains here.

The `ZipList` applicative 
Lists with a designated padding element are applicative (the applicative grown from the 1 and max monoid structure on positive numbers).

    data Padme m = (:-) {padded :: [m], padder :: m} deriving (Show, Eq)

    instance Applicative Padme where
      pure = ([] :-)
      (fs :- f) <*> (ss :- s) = zapp fs ss :- f s where
        zapp  []        ss        = map f ss
        zapp  fs        []        = map ($ s) fs
        zapp  (f : fs)  (s : ss)  = f s : zapp fs ss

    -- and for those of you who don't have DefaultSuperclassInstances
    instance Functor Padme where fmap = (<*>) . pure

Now we can pack up lists of numbers with their appropriate padding

    pad0 :: [Int] -> Padme Int
    pad0 = (:- 0)

And that gives

    padded ((\x y z -> x+y+z) <$> pad0 [1,2,3] <*> pad0 [4,5] <*> pad0 [6])
    = [11,7,3]

Or, with the Idiom Brackets that aren't available, you vould write

    padded (|pad0 [1,2,3] + (|pad0 [4,5] + pad0 6|)|)

meaning the same.

`Applicative` gives you a good way to bottle the essential idea of "padding" that this problem demands.

  [1]: https://stackoverflow.com/a/21350096/828361
