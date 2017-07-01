Some monads have a sensible failure mechanism, e.g. the terminal monad:

    data Fail x = Fail

Some monads don't have a sensible failure mechanism (`undefined` is not sensible), e.g. the initial monad:

    data Return x = Return x

In that sense, it's clearly a wart to require all monads to have a `fail` method. If you're writing programs that abstract over monads `(Monad m) =>`, it's not very healthy to make use of that generic `m`'s `fail` method. That would result in a function you can instantiate with a monad where `fail` shouldn't really exist.

I see fewer objections to using `fail` (especially indirectly, by matching `Pat <- computation`) when working in a specific monad for which a good `fail` behaviour has been clearly specified. Such programs would hopefully survive a return to the old discipline where nontrivial pattern matching created a demand for `MonadZero` instead of just `Monad`.

One might argue that the better discipline is always to treat failure-cases explicitly. I object to this position on two counts: (1) that the point of monadic programming is to avoid such clutter, and (2) that the current notation for case analysis on the result of a monadic computation is so awful. The next release of SHE will support the notation (also found in other variants)

    case <- computation of
      Pat_1 -> computation_1
      ...
      Pat_n -> computation_n

which might help a little.

But this whole situation is a sorry mess. It's often helpful to characterize monads by the operations which they support. You can see `fail`, `throw`, etc as operations supported by some monads but not others. Haskell makes it quite clumsy and expensive to support small localized changes in the set of operations available, introducing new operations by explaining how to handle them in terms of the old ones. If we seriously want to do a neater job here, we need to rethink how `catch` works, to make it a translator between *different* local error-handling mechanisms. I often want to bracket a computation which can fail uninformatively (e.g. by pattern match failure) with a handler that adds more contextual information before passing on the error. I can't help feeling that it's sometimes more difficult to do that than it should be.

So, this is a could-do-better issue, but at the very least, use `fail` only for specific monads which offer a sensible implementation, and handle the 'exceptions' properly.
