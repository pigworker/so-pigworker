Suppose we have

    pure :: forall r a. a -> (r, a)

then, in particular, we have

    magic :: forall r. r
    magic = fst (pure ())

Now, we can specialise the type variable `r` to get

    magic :: Void

where `Void` is the datatype with no constructors, which means

    magic = undefined

but as type variables (and the types which specialise them) play no run time role, that means `magic` is *always* undefined.

We've discovered that `((,) r)` can be `Applicative` only for *inhabited* `r`. And there's more. With any such instance, we can write

    munge :: r -> r -> r
    munge r0 r1 = fst ( pure (\ _ _ -> ()) <*> (r0, ()) <*> (r1, ()) )

to define a binary operator on `r`. The `Applicative` laws tell us effectively that `munge` must be an associative operator that absorbs `magic` on either side.

That's to say there *is* a sensible instance

    instance Monoid r => Applicative ((,) r) where
      pure a              = (mempty, a)
      (r0, f) <*> (r1, s) = (mappend r0 r1, f s)

(exactly what you get when you take `pure=return; (<*>)=ap` from the `Monad (Writer r)`).

Of course, some pedants would argue that it is legal (if unhelpful) to define

    instance Monoid r where
      mempty = undefined
      mappend _ _ = undefined
      -- Monoid laws clearly hold

but I would argue that any sensible type class instance should contribute nontrivially to the defined fragment of the language.
