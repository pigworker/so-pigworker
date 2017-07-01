Type inference is, by default, a guessing game. Haskell's surface syntax makes it rather awkward to be explicit about which types should instantiate a `forall`, even if you know what you want. This is a legacy from the good old days of Damas-Milner completeness, when ideas interesting enough to require explicit typing were simply forbidden.

Let's imagine we're allowed to make type application explicit in patterns and expressions using an Agda-style `f {a = x}` notation, selectively accessing the type parameter corresponding to `a` in the type signature of `f`. Your

    idT = StateT $ \ s -> idT

is supposed to mean

    idT {a = a}{m = m} = StateT $ \ s -> idT {a = a}{m = m}

so that the left has type `C a a (StateT s m) r` and the right has type `StateT s (C a a m) r`, which are equal by definition of the type family and joy radiates over the world. But that's not the meaning of what you wrote. The "variable-rule" for invoking polymorphic things requires that each `forall` is instantiated with a fresh existential type variable which is then solved by unification. So what your code means is

    idT {a = a}{m = m} = StateT $ \ s -> idT {a = a'}{m = m'}
      -- for a suitably chosen a', m'

The available constraint, after computing the type family, is

    C a a m ~ C a' a' m'

but that doesn't simplify, nor should it, because there is no reason to assume `C` is injective. And the maddening thing is that the machine cares more than you about the possibility of finding a most general solution. You have a suitable solution in mind already, but the problem is to achieve *communication* when the default assumption is *guesswork*.

There are a number of strategies which might help you out of this jam. One is to use data families instead. Pro: injectivity no problem. Con: structor. (Warning, untested speculation below.)

    class MonadPipe m where
      data C a b (m :: * -> *) r
      idT :: C a a m r
      (<-<) :: C b c m r -> C a b m r -> C a c m r

    instance (MonadPipe m) => MonadPipe (StateT s m) where
      data C a b (StateT s m) r = StateTPipe (StateT s (C a b m) r)
      idT = StateTPipe . StateT $ \ s -> idT
      StateTPipe (StateT f) <-< StateTPipe (StateT g) =
        StateTPipe . StateT $ \ s - f s <-< g s

Another con is that the resulting data family is not automatically monadic, nor is it so very easy to unwrap it or make it monadic in a uniform way.

I'm thinking of trying out a pattern for these things where you keep your type family, but define a newtype wrapper for it

    newtype WrapC a b m r = WrapC {unwrapC :: C a b m r}

then use `WrapC` in the types of the operations to keep the typechecker on the right track. I don't know if that's a good strategy, but I plan to find out, one of these days.

A more direct strategy is to use proxies, phantom types, and scoped type variables (although this example shouldn't need them). (Again, speculation warning.)

    data Proxy (a :: *) = Poxy
    data ProxyF (a :: * -> *) = PoxyF

    class MonadPipe m where
      data C a b (m :: * -> *) r
      idT :: (Proxy a, ProxyF m) -> C a a m r
      ...

    instance (MonadPipe m) => MonadPipe (StateT s m) where
      data C a b (StateT s m) r = StateTPipe (StateT s (C a b m) r)
      idT pp = StateTPipe . StateT $ \ s -> idT pp

That's just a crummy way of making the type applications explicit. Note that some people use `a` itself instead of `Proxy a` and pass `undefined` as the argument, thus failing to mark the proxy as such in the type and relying on not accidentally evaluating it. Recent progress with `PolyKinds` may at least mean we can have just one kind-polymorphic phantom proxy type. Crucially, the `Proxy` type constructors are injective, so my code really is saying "same parameters here as there".

But there are times when I wish that I could drop to the System FC level in source code, or otherwise express a clear manual override for type inference. Such things are quite standard in the dependently typed community, where nobody imagines that the machine can figure everything out without a nudge here and there, or that hidden arguments carry no information worth inspecting. It's quite common that hidden arguments to a function can be suppressed at usage sites but need to be made explicit within the definition. The present situation in Haskell is based on a cultural assumption that "type inference is enough" which has been off the rails for a generation but still somehow persists.
