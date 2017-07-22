Let's remind ourselves about tries. (Lots of my mates were working on these structures in the early noughties: Ralf Hinze, Thorsten Altenkirch and Peter Hancock spring instantly to mind in that regard.) What's really going on is that we're computing the exponential of a type `t`, remembering that `t -> x` is a way of writing `x` ^ `t`.

That is, we expect to equip a type `t` with a functor `Expo t` such that `Expo t x` represents `t -> x`. We should further expect `Expo t` to be applicative (zippily).

    class Applicative (Expo t) => EXPO t where
      type Expo t :: * -> *
      appl  :: Expo t x -> (t -> x)       -- trie lookup
      abst  :: (t -> x) -> Expo t x       -- trie construction

Another way of putting it is that `t` is the *logarithm* of `Expo t`.

(I nearly forgot: fans of calculus should check that `t` is isomorphic to `∂ (Expo t) ()`. This isomorphism might actually be rather useful.)

We'll need some functor kit stuff. The identity functor is zippiy applicative...

    data I     ::                         (* -> *) where
      I   :: x -> I x
      deriving (Show, Eq, Functor, Foldable, Traversable)

    instance Applicative I where
      pure x = I x
      I f <*> I s = I (f s)

...and its logarithm is the unit type

    instance EXPO () where
      type Expo () = I
      appl (I x) () = x
      abst f        = I (f ())

Products of zippy applicatives are zippily applicative...

    data (:*:) :: (* -> *) -> (* -> *) -> (* -> *) where
      (:*:) :: f x -> g x -> (f :*: g) x
      deriving (Show, Eq, Functor, Foldable, Traversable)

    instance (Applicative p, Applicative q) => Applicative (p :*: q) where
      pure x = pure x :*: pure x
      (pf :*: qf) <*> (ps :*: qs) = (pf <*> ps) :*: (qf <*> qs)

...and their logarithms are sums.

    instance (EXPO s, EXPO t) => EXPO (Either s t) where
      type Expo (Either s t) = Expo s :*: Expo t
      appl (sf :*: tf) (Left s)  = appl sf s
      appl (sf :*: tf) (Right t) = appl tf t
      abst f = abst (f . Left) :*: abst (f . Right)

Compositions of zippy applicatives are zippily applicative...

    data (:<:) :: (* -> *) -> (* -> *) -> (* -> *) where
      C :: f (g x) -> (f :<: g) x
      deriving (Show, Eq, Functor, Foldable, Traversable)

    instance (Applicative p, Applicative q) => Applicative (p :<: q) where
      pure x          = C (pure (pure x))
      C pqf <*> C pqs = C (pure (<*>) <*> pqf <*> pqs)

and their logarithms are products.

    instance (EXPO s, EXPO t) => EXPO (s, t) where
      type Expo (s, t) = Expo s :<: Expo t
      appl (C stf) (s, t) = appl (appl stf s) t
      abst f = C (abst $ \ s -> abst $ \ t -> f (s, t))

If we switch on enough stuff, we may now write

    newtype Tree    = Tree (Either () (Tree, Tree))
      deriving (Show, Eq)
    newtype ExpoTree x = ExpoTree (Expo (Either () (Tree, Tree)) x)
      deriving (Show, Eq, Functor, Foldable, Traversable, Applicative)

    instance EXPO Tree where
      type Expo Tree = ExpoTree
      appl (ExpoTree f) (Tree t) = appl f t
      abst f = ExpoTree (abst (f . Tree))

The `TreeMap a` type in the question, being

    data TreeMap a
        = TreeMap {
            tm_leaf :: Maybe a,
            tm_node :: TreeMap (TreeMap a)
          }

is exactly `Expo Tree (Maybe a)`, with `lookupTreeMap` being `flip appl`.

Now, given that `Tree` and `Tree -> x` are rather different things, it strikes me as odd to want code to work "on both". The tree equality test is a special case of the lookup only in that the tree equality test is any old function which acts on a tree. There is a coincidence coincidence, however: to test equality, we must turn each tree into own self-recognizer.

The structure which gives rise to an equality test is some notion of *matching*. Like this:

    class Matching a b where
      type Matched a b :: *
      matched :: Matched a b -> (a, b)
      match   :: a -> b -> Maybe (Matched a b)

That is, we expect `Matched a b` to represent somehow a pair of an `a` and a `b` which match. We should be able to extract the pair (forgetting that they match), and we should be able to take any pair and try to match them.

Unsurprisingly, we can do this for the unit type, quite successfully.

    instance Matching () () where
      type Matched () () = ()
      matched () = ((), ())
      match () () = Just ()

For products, we work componentwise, with component mismatch being the only danger.

    instance (Matching s s', Matching t t') => Matching (s, t) (s', t') where
      type Matched (s, t) (s', t') = (Matched s s', Matched t t')
      matched (ss', tt') = ((s, t), (s', t')) where
        (s, s') = matched ss'
        (t, t') = matched tt'
      match (s, t) (s', t') = (,) <$> match s s' <*> match t t'

Sums offer some chance of mismatch.

    instance (Matching s s', Matching t t') =>
        Matching (Either s t) (Either s' t') where
      type Matched (Either s t) (Either s' t')
        = Either (Matched s s') (Matched t t')
      matched (Left  ss') = (Left s,  Left s')  where (s, s') = matched ss'
      matched (Right tt') = (Right t, Right t') where (t, t') = matched tt'
      match (Left s)  (Left s')  = Left  <$> match s s'
      match (Right t) (Right t') = Right <$> match t t'
      match _         _          = Nothing

Amusingly, we can obtain an equality test for trees now as easily as

    instance Matching Tree Tree where
      type Matched Tree Tree = Tree
      matched t = (t, t)
      match (Tree t1) (Tree t2) = Tree <$> match t1 t2

(Incidentally, the `Functor` subclass that captures a notion of matching, being

    class HalfZippable f where  -- "half zip" comes from Roland Backhouse
      halfZip :: (f a, f b) -> Maybe (f (a, b))

is sadly neglected. Morally, for each such `f`, we should have

    Matched (f a) (f b) = f (Matched a b)

A fun exercise is to show that if `(Traversable f, HalfZippable f)`, then the free monad on `f` has a first-order unification algorithm.)

I suppose we can build "singleton association lists" like this:

    mapOne :: forall a. (Tree, a) -> Expo Tree (Maybe a)
    mapOne (t, a) = abst f where
      f :: Tree -> Maybe a
      f u = pure a <* match t u

And we could try combining them with this gadget, exploiting the zippiness of all the `Expo t`s...

    instance Monoid x => Monoid (ExpoTree x) where
      mempty = pure mempty
      mappend t u = mappend <$> t <*> u

...but, yet again, the utter stupidity of the `Monoid` instance for `Maybe x` continues to frustrate clean design.

We can at least manage

    instance Alternative m => Alternative (ExpoTree :<: m) where
      empty = C (pure empty)
      C f <|> C g = C ((<|>) <$> f <*> g)

An amusing exercise is to fuse `abst` with `match`, and perhaps that's what the question is really driving at. Let's refactor `Matching`.

    class EXPO b => Matching a b where
      type Matched a b :: *
      matched :: Matched a b -> (a, b)
      match'  :: a -> Proxy b -> Expo b (Maybe (Matched a b))

    data Proxy x = Poxy  -- I'm not on GHC 8 yet, and Simon needs a hand here

For `()`, what's new is

    instance Matching () () where
      -- skip old stuff
      match' () (Poxy :: Proxy ()) = I (Just ())

For sums, we need to tag successful matches, and fill in the unsuccessful parts with a magnificently Glaswegian `pure Nothing`.

    instance (Matching s s', Matching t t') =>
        Matching (Either s t) (Either s' t') where
      -- skip old stuff
      match' (Left s) (Poxy :: Proxy (Either s' t')) =
        ((Left <$>) <$> match' s (Poxy :: Proxy s')) :*: pure Nothing
      match' (Right t) (Poxy :: Proxy (Either s' t')) =
        pure Nothing :*: ((Right <$>) <$> match' t (Poxy :: Proxy t'))
     
For pairs, we need to build matching in sequence, dropping out early if the
first component fails.

    instance (Matching s s', Matching t t') => Matching (s, t) (s', t') where
      -- skip old stuff
      match' (s, t) (Poxy :: Proxy (s', t'))
        = C (more <$> match' s (Poxy :: Proxy s')) where
        more Nothing  = pure Nothing
        more (Just s) = ((,) s <$>) <$> match' t (Poxy :: Proxy t')

So we can see that there is a connection between a constructor and the trie for its matcher.

Homework: fuse `abst` with `match'`, effectively tabulating the entire process.
