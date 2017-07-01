There are at least three relevant aspects to this question.

  1. Given a `Monad m` instance, what is the specification of its necessary `Applicative m` superclass instance? *Answer*: `pure` is `return`, `<*>` is `ap`, so

        mf <*> ms == do f <- mf; s <- ms; return (f s)

Note that this specification is not a law of the `Applicative` class. It's a requirement on `Monad`s, to ensure consistent usage patterns.

  2. Given that specification (by candidate implementation), is `ap` the only acceptable implementation. *Answer*: resoundingly, **no**. The value dependency permitted by the type of `>>=` can sometimes lead to inefficient execution: there are situations where `<*>` can be made more efficient than `ap` because you don't need to wait for the first computation to finish before you can tell what the second computation is. The "applicative do" notation exists exactly to exploit this possibility.

  3. Do any other candidate instances for `Applicative` satisfy the `Applicative` laws, even though they disagree with the required `ap` instances? *Answer*: yes. The "backwards" instance proposed by the question is just such a thing. Indeed, as another answer observes, any applicative can be turned backwards, and the result is often a different beast.

For a further example and exercise for the reader, note that nonempty lists are monadic in the way familiar from ordinary lists.

      data Nellist x = x :& Maybe (Nellist x)

      necat :: Nellist x -> Nellist x -> Nellist x
      necat (x :& Nothing) ys = x :& Just ys
      necat (x :& Just xs) ys = x :& Just (necat xs ys)

      instance Monad Nellist where
        return x = x :& Nothing
        (x :& Nothing) >>= k = k x
        (x :& Just xs) >>= k = necat (k x) (xs >>= k)

Find at least *four* behaviourally distinct instances of `Applicative Nellist` which obey the applicative laws.
