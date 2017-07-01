When interpreted sufficiently pedantically, the answer to all of these questions is "yes", but for uninformatively trivial reasons.

Every category C restricts to a discrete subcategory |C| with the same objects as C but only identity morphisms (and hence no interesting structure). At the very least, operations on Haskell types can be boringly interpreted as operations on the discrete category `|*|`. The recent "roles" story amounts to (but is not spun as) an attempt to acknowledge that the morphisms matter, not just the objects. The "nominal" role for types amounts to working in `|*|` rather than `*`.

(Note, I dislike the use of "Hask" as the name of the "category of Haskell types and functions": I fear that labelling one category as *the* Haskell category has the unfortunate side-effect of blinding us to the wealth of *other* categorical structure in Haskell programming. It's a trap.)

Being differently pedantic, I'd note that you can make up any old crap as a typeclass over any old kind, with no interesting structure whatsoever (but with trivial structure that can still be talked about categorically, if one must). However, the classes you find in the library are very often structure-rich. Classes over `* -> *` are often, by design, subclasses of `Functor`, requiring the existence of certain natural transformations in addition to `fmap`.

For question 2. Yes, of course a class over `*` gives a subcategory of `*`. It's no problem to chuck objects out of a category, because the categorical requirement that identities and composites exist require *morphisms* to exist, given objects, but make no demands about which *objects* exist. The fact that it's boringly possible makes it a boring fact. However, many Haskell typeclasses over `*` give rise to much more interesting categories than those arising just as subcategories of `*`. E.g., the `Monoid` class gives us a category where the objects are instances of `Monoid` and the arrows are *monoid homomorphisms*: not just any old function `f` from one `Monoid` to another, but one which preserves the structure: `f mempty = mempty` and `f (mappend x y) = mappend (f x) (f y)`.

For question 3, well, in that there's a ton of categorical structure lurking everywhere, there's certainly a ton of categorical structure available (possibly but not necessarily) at higher kinds. I'm particularly fond of functors between indexed families of sets.

    type (s :: k -> *) :-> (t :: k -> *) = forall x. s x -> t x

    class FunctorIx (f :: (i -> *) -> (j -> *)) where
      mapIx :: (s :-> t) -> (f s :-> f t)

When `i` and `j` coincide, it becomes sensible to ask when such an `f` is a monad. The usual categorical definition suffices, even though we've left `* -> *` behind.

The message is this: nothing about being a typeclass inherently induces *interesting* categorical structure; there is plenty of interesting categorical structure which can usefully be presented via type classes over all manner of kinds. There are most certainly interesting functors from `*` (sets and functions) to `* -> *` (functors and natural transformations). Don't be blinded by careless talk about "Hask" to the richness of categorical structure in Haskell.
