It seems amusing to post some questions as an answer. This is a fun one, on the interplay between `Applicative` and `Traversable`, based on sudoku.

(1) Consider

    data Triple a = Tr a a a

Construct

    instance Applicative Triple
    instance Traversable Triple

so that the `Applicative` instance does "vectorization" and the `Traversable` instance works left-to-right. Don't forget to construct a suitable `Functor` instance: check that you can extract this from either of the `Applicative` or the `Traversable` instance. You may find

    newtype I x = I {unI :: x}

useful for the latter.

(2) Consider

    newtype (:.) f g x = Comp {comp :: f (g x)}

Show that

    instance (Applicative f, Applicative g) => Applicative (f :. g)
    instance (Traversable f, Traversable g) => Traversable (f :. g)

Now define

    type Zone = Triple :. Triple

Suppose we represent a `Board` as a vertical zone of horizontal zones

    type Board = Zone :. Zone

Show how to rearrange it as a horizontal zone of vertical zones, and as a square of squares, using the functionality of `traverse`.

(3) Consider

    newtype Parse x = Parser {parse :: String -> [(x, String)]} deriving Monoid

or some other suitable construction (noting that the library `Monoid` behaviour for |Maybe| is inappropriate). Construct

    instance Applicative Parse
    instance Alternative Parse  -- just follow the `Monoid`

and implement

    ch :: (Char -> Bool) -> Parse Char

which consumes and delivers a character if it is accepted by a given predicate.

(4) Implement a parser which consumes any amount of whitespace, followed by a single digit (0 represents blanks)

    square :: Parse Int

Use `pure` and `traverse` to construct

    board :: Parse (Board Int)

(5) Consider the constant functors

    newtype K a x = K {unK :: a}

and construct

    instance Monoid a => Applicative (K a)

then use `traverse` to implement

    crush :: (Traversable f, Monoid b) => (a -> b) -> f a -> b

Construct `newtype` wrappers for `Bool` expressing its conjunctive and disjunctive monoid structures. Use `crush` to implement versions of `any` and `all` which work for any `Traversable` functor.

(6) Implement

    duplicates :: (Traversable f, Eq a) => f a -> [a]

computing the list of values which occur more than once. (Not completely trivial.) (There's a lovely way to do this using differential calculus, but that's another story.)

(7) Implement

    complete :: Board Int -> Bool
    ok :: Board Int -> Bool

which check if a board is (1) full only of digits in [1..9] and (2) devoid of duplicates in any row, column or box.
