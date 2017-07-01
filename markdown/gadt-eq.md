As others have identified, the key to the problem is the existentially quantified `tag` in the type of `Con3`. When you're trying to define

    Con3 s == Con3 t = ???

there's no reason why `s` and `t` should be expressions with the same `tag`.

But perhaps you don't care? You can perfectly well define the *heterogeneous* equality test which is happy to compare `Expr`s structurally, regardless of tags.

    instance Eq (Expr tag) where
      (==) = heq where
        heq :: Expr a -> Expr b -> Bool
        heq (Con1 i) (Con1 j) = i == j
        heq (Con2 s) (Con2 t) = heq s t
        heq (Con3 s) (Con3 t) = heq s t

If you do care, then you might be well advised to equip `Con3` with a run-time witness to the existentially quantified `tag`. The standard way to do this is with the *singleton* construction.

    data SingExprTag (tag :: ExprTag) where
      SingTag1 :: SingExprTag Tag1
      SingTag2 :: SingExprTag Tag2

Case analysis on a value in `SingExprTag tag` will exactly determine what `tag` is. We can slip this extra piece of information into `Con3` as follows:

    data Expr' (tag :: ExprTag) where
      Con1' :: Int -> Expr' tag
      Con2' :: Expr' tag -> Expr' tag
      Con3' :: SingExprTag tag -> Expr' tag -> Expr' Tag2

Now we can check whether the tags match. We could write a heterogeneous equality for tag singletons like this...

    heqTagBoo :: SingExprTag a -> SingExprTag b -> Bool
    heqTagBoo SingTag1 SingTag1 = True
    heqTagBoo SingTag2 SingTag2 = True
    heqTagBoo _        _        = False

...but to do so would be perfectly useless, as it only gives us a value of type `Bool`, with no idea what that value means nor to what its truth might entitle us. Knowing that `heqTagBoo a b = True` does not tell the typechecker anything useful about the tags which `a` and `b` witness. **A Boolean is a bit uninformative.**

We can write essentially the same test, but delivering in the positive case some *evidence* that the tags are equal.

    data x :=: y where
      Refl :: x :=: x

    singExprTagEq :: SingExprTag a -> SingExprTag b -> Maybe (a :=: b)
    singExprTagEq SingTag1 SingTag1  = Just Refl
    singExprTagEq SingTag2 SingTag2  = Just Refl
    singExprTagEq _        _         = Nothing

Now we're cooking with gas! We can implement an instance of `Eq` for `Expr'` which uses `ExprTag` comparison to justify a recursive call on two `Con3'` children when the tags have been shown to match.

    instance Eq (Expr' tag) where
      Con1' i    ==  Con1' j    = i == j
      Con2' s    ==  Con2' t    = s == t
      Con3' a s  ==  Con3' b t  = case singExprTagEq a b of
        Just Refl -> s == t
        Nothing -> False

The general situation is that promoted types need their associated singleton types (at least until we get proper ∏-types), and we need evidence-producing heterogeneous equality tests for those singleton families, so that we can compare two singletons and gain type-level knowledge when they witness the same type-level values. Then as long as your GADTs carry singleton witnesses for any existentials, you can test equality homogeneously, ensuring that positive results from singleton tests give the bonus of unifying types for the other tests.
