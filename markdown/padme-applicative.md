There is some structure to this problem, and here it comes. I'll be using this stuff:

    import Control.Applicative
    import Data.Traversable
    import Data.List

First up, lists-with-padding are a useful concept, so let's have a type for them.

    data Padme m = (:-) {padded :: [m], padder :: m} deriving (Show, Eq)

Next, I remember that the truncating-`zip` operation gives rise to an `Applicative` instance, in the library as `newtype ZipList` (a popular example of a non-`Monad`). The `Applicative ZipList` amounts to a decoration of the monoid given by infinity and minimum. `Padme` has a similar structure, except that its underlying monoid is positive numbers (with infinity), using one and maximum.

    instance Applicative Padme where
      pure = ([] :-)
      (fs :- f) <*> (ss :- s) = zapp fs ss :- f s where
        zapp  []        ss        = map f ss
        zapp  fs        []        = map ($ s) fs
        zapp  (f : fs)  (s : ss)  = f s : zapp fs ss

I am obliged to utter the usual incantation to generate a default `Functor` instance.

    instance Functor Padme where fmap = (<*>) . pure

Thus equipped, we can pad away! For example, the function which takes a ragged list of strings and pads them with spaces becomes a one liner.

    deggar :: [String] -> [String]
    deggar = transpose . padded . traverse (:- ' ')

See?

    *Padme> deggar ["om", "mane", "padme", "hum"]
    ["om   ","mane ","padme","hum  "]
