Tags of tagged unions ought to be first-class values, and with a wee bit of effort, they are.

Jiggery-pokery alert:

    {-# LANGUAGE GADTs, DataKinds, KindSignatures,
        TypeFamilies, PolyKinds, FlexibleInstances,
        PatternSynonyms
    #-}

Step one: define type-level versions of the tags.

    data TagType = EmptyTag | SingleTag | PairTag | LotsTag

Step two: define value-level witnesses for the representability of the type-level tags. Richard Eisenberg's Singletons library will do this for you. I mean something like this:

    data Tag :: TagType -> * where
      EmptyT   :: Tag EmptyTag
      SingleT  :: Tag SingleTag
      PairT    :: Tag PairTag
      LotsT    :: Tag LotsTag

And now we can say what stuff we expect to find associated with a given tag.

    type family Stuff (t :: TagType) :: * where
      Stuff EmptyTag   = ()
      Stuff SingleTag  = Int
      Stuff PairTag    = (Int, Int)
      Stuff LotsTag    = [Int]

So we can refactor the type you first thought of

    data NumCol :: * where
      (:&) :: Tag t -> Stuff t -> NumCol

and use `PatternSynonyms` to recover the behaviour you had in mind:

    pattern Empty        = EmptyT   :&  ()
    pattern Single  i    = SingleT  :&  i
    pattern Pair    i j  = PairT    :&  (i, j)
    pattern Lots    is   = LotsT    :&  is

So what's happened is that each constructor for `NumCol` has turned into a tag indexed by the kind of tag it's for. That is, constructor tags now live separately from the rest of the data, synchronized by a common index which ensures that the stuff associated with a tag matches the tag itself.

But we can talk about tags alone.

    data Ex :: (k -> *) -> * where  -- wish I could say newtype here
      Witness :: p x -> Ex p

Now, `Ex Tag`, is the type of "runtime tags with a type level counterpart". It has an `Eq` instance

    instance Eq (Ex Tag) where
      Witness EmptyT   ==  Witness EmptyT   = True
      Witness SingleT  ==  Witness SingleT  = True
      Witness PairT    ==  Witness PairT    = True
      Witness LotsT    ==  Witness LotsT    = True
      _                ==  _                = False

Moreover, we can easily extract the tag of a `NumCol`.

    numColTag :: NumCol -> Ex Tag
    numColTag (n :& _) = Witness n

And that allows us to match your specification.

    filter ((Witness PairT ==) . numColTag) :: [NumCol] -> [NumCol]

Which raises the question of whether your specification is actually what you need. The point is that detecting a tag entitles you an expectation of that tag's stuff. The output type `[NumCol]` doesn't do justice to the fact that you know you have just the pairs.

How might you tighten the type of your function and still deliver it?
