Quite right to construct a recursive functor by taking the fixpoint of a bifunctor, because 1 + 1 = 2. The list node structure is given as a container with 2 sorts of substructure: "elements" and "sublists".

It can be troubling that we need a whole other notion of `Functor` (which captures a rather specific variety of functor, despite its rather general name), to construct a `Functor` as a fixpoint. We can, however (as a bit of a stunt), shift to a slightly more general notion of functor which is *closed* under fixpoints.

    type p -:> q = forall i. p i -> q i

    class FunctorIx (f :: (i -> *) -> (o -> *)) where
      mapIx :: (p -:> q) -> f p -:> f q

These are the functors on *indexed sets*, so the names are not just gratuitous homages to Goscinny and Uderzo. You can think of `o` as "sorts of structure" and `i` as "sorts of substructure". Here's an example, based on the fact that 1 + 1 = 2.

    data ListF :: (Either () () -> *) -> (() -> *) where
      Nil  :: ListF p '()
      Cons :: p (Left '()) -> p (Right '()) -> ListF p '()

    instance FunctorIx ListF where
      mapIx f Nil        = Nil
      mapIx f (Cons a b) = Cons (f a) (f b)

To exploit the choice of substructure sort, we'll need a kind of type-level case analysis. We can't get away with a type function, as

  1. we need it to be partially applied, and that's not allowed;
  2. we need a bit at run time to tell us which sort is present.

<!-- gaah -->

    data Case :: (i -> *) -> (j -> *) -> (Either i j -> *)  where
      CaseL :: p i -> Case p q (Left i)
      CaseR :: q j -> Case p q (Right j)

    caseMap :: (p -:> p') -> (q -:> q') -> Case p q -:> Case p' q'
    caseMap f g (CaseL p) = CaseL (f p)
    caseMap f g (CaseR q) = CaseR (g q)

And now we can take the fixpoint:

    data Mu :: ((Either i j -> *) -> (j -> *)) ->
               ((i -> *) -> (j -> *)) where
      In :: f (Case p (Mu f p)) j -> Mu f p j

In each substructure position, we do a case split to see whether we should have a `p`-element or a `Mu f p` substructure. And we get its functoriality.

    instance FunctorIx f => FunctorIx (Mu f) where
      mapIx f (In fpr) = In (mapIx (caseMap f (mapIx f)) fpr)

To build lists from these things, we need to juggle between `*` and `() -> *`.

    newtype K a i = K {unK :: a}

    type List a = Mu ListF (K a) '()
    pattern NilP :: List a
    pattern NilP       = In Nil
    pattern ConsP :: a -> List a -> List a
    pattern ConsP a as = In (Cons (CaseL (K a)) (CaseR as))

Now, for lists, we get

    map' :: (a -> b) -> List a -> List b
    map' f = mapIx (K . f . unK)
