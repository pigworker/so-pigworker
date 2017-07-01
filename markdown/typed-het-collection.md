One way to approach this problem is to tag values with run-time type representatives. I'm channelling Stephanie Weirich, here. Let's have a small example. First, give a representation to some types. That's typically done with a *singleton* construction.

    data Type :: * -> * where
      Int   :: Type Int
      Char  :: Type Char
      List  :: Type x -> Type [x]

So `Type Int` contains one value, which I've also called `Int`, because it acts as the run-time representative of the type `Int`. If you can see colour even in monochrome things, the `Int` left of the `::` is red, and the `Int` after `Type` is blue.

Now we can do existential packaging, preserving utility.

    data Cell :: * where
     (:::) :: x -> Type x -> Cell

A `Cell` is a value tagged with a run-time representative of its type. You can recover the utility of the value by reading its type tag. Indeed, as types are first-order structures, we can check them for equality in a useful way.

    data EQ :: k -> k -> * where
      Refl :: EQ x x

    typeEQ :: Type x -> Type y -> Maybe (EQ x y)
    typeEQ Int Int = Just Refl
    typeEQ Char Char = Just Refl
    typeEQ (List s) (List t) = case typeEQ s t of
      Just Refl -> Just Refl
      Nothing -> Nothing
    typeEQ _ _ = Nothing

A Boolean equality on type representatives is no use: we need the equality test to construct the *evidence* that the represented types can be unified. With the evidence-producing test, we can write

    gimme :: Type x -> Cell -> Maybe x
    gimme t (x ::: s) = case typeEQ s t of
      Just Refl -> Just x
      Nothing   -> Nothing

Of course, writing the type tags is a nuisance. But why keep a dog and bark yourself?

    class TypeMe x where
      myType :: Type x

    instance TypeMe Int where
      myType = Int

    instance TypeMe Char where
      myType = Char

    instance TypeMe x => TypeMe [x] where
      myType = List myType

    cell :: TypeMe x => x -> Cell
    cell x = x ::: myType

And now we can do things like

    myCells :: [Cell]
    myCells = [cell (length "foo"), cell "foo"]

and then get

    > gimme Int (head myCells)
    Just 3

Of course, it would all be so much tidier if we didn't have to do the singleton construction and could just pattern-match on such types as we might choose to retain at run-time. I expect we'll get there when the mythical `pi` quantifier becomes less mythical.
