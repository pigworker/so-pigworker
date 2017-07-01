Is there some particular reason why the *Kleene star* GADT won't do this job?

    data Star r a b where
      Nil   :: Star r a a
      Cons  :: r a b -> Star r b c -> Star r a c

    compose :: Star (->) a b -> a -> b
    compose Nil          = id
    compose (Cons f fs)  = compose fs . f

But if you need a type class approach, I wouldn't interfere.
