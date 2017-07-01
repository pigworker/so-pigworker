Every valid `Traversable f` is isomorphic to `Normal s` for some `s :: Nat -> *` where

    data Normal (s :: Nat -> *) (x :: *) where  -- Normal is Girard's terminology
      (:-) :: s n -> Vec n x -> Normal s x

    data Nat = Zero | Suc Nat

    data Vec (n :: Nat) (x :: *) where
      Nil   :: Vec Zero n
      (:::) :: x -> Vec n x -> Vec (Suc n) x

but it's not at all trivial to implement the iso in Haskell (but it's worth a go with full dependent types). Morally, the `s` you pick is

    data {- not really -} ShapeSize (f :: * -> *) (n :: Nat) where
      Sized :: pi (xs :: f ()) -> ShapeSize f (length xs)

and the two directions of the iso separate and recombine shape and contents. The shape of a thing is given just by `fmap (const ())`, and the key point is that the length of the shape of an `f x` is the length of the `f x` itself.

Vectors are traversable in the visit-each-once-left-to-right sense. Normals are traversable exactly in by preserving the shape (hence the size) and traversing the vector of elements. To be traversable is to have finitely many element positions arranged in a linear order: isomorphism to a normal functor exactly exposes the elements in their linear order. Correspondingly, every `Traversable` structure is a (finitary) container: they have a set of shapes-with-size and a corresponding notion of position given by the initial segment of the natural numbers strictly less than the size.

The `Foldable` things are also finitary and they keep things in an order (there is a sensible `toList`), but they are not guaranteed to be `Functor`s, so they don't have such a crisp notion of *shape*. In that sense (the sense of "container" defined by my colleagues Abbott, Altenkirch and Ghani), they do not necessarily admit a shapes-and-positions characterization and are thus not containers. If you're lucky, some of them may be containers upto some quotient. Indeed `Foldable` exists to allow processing of structures like `Set` whose internal structure is intended to be a secret, and certainly depends on ordering information about the elements which is not necessarily respected by traversing operations. Exactly what constitutes a well behaved `Foldable` is rather a moot point, however: I won't quibble with the pragmatic benefits of that library design choice, but I could wish for a clearer specification.
