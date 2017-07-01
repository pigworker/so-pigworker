So I thought about it a bit more and made some progress. Here's a first stab at encoding Martin-Löf's delightfully simple (but inconsistent) `Set : Set` system in a combinatory style. It's not a good way to finish, but it's the easiest place to get started. The syntax of this type theory is just lambda-calculus with type annotations, Pi-types, and a universe Set.

### The Target Type Theory ###

For completeness' sake, I'll present the rules. Context validity just says you can build contexts from empty by adjoining fresh variables inhabiting `Set`s. 

                         G |- valid   G |- S : Set
    --------------     ----------------------------- x fresh for G
      . |- valid         G, x:S |- valid

And now we can say how to synthesize types for terms in any given context, and how to change the type of something up to the computational behaviour of the terms it contains.

      G |- valid             G |- S : Set   G |- T : Pi S \ x:S -> Set
    ------------------     ---------------------------------------------
      G |- Set : Set         G |- Pi S T : Set

      G |- S : Set   G, x:S |- t : T x         G |- f : Pi S T   G |- s : S
    ------------------------------------     --------------------------------
      G |- \ x:S -> t : Pi S T                 G |- f s : T s

      G |- valid                  G |- s : S   G |- T : Set
    -------------- x:S in G     ----------------------------- S ={beta} T
      G |- x : S                  G |- s : T

In a small variation from the original, I've made lambda the only binding operator, so the second argument of Pi should be a function computing the way the return type depends on the input. By convention (e.g. in Agda, but sadly not in Haskell), scope of lambda extends rightwards as far as possible, so you can often leave abstractions unbracketed when they're the last argument of a higher-order operator: you can see I did that with Pi. Your Agda type `(x : S) -> T` becomes `Pi S \ x:S -> T`.

(*Digression*. Type annotations on lambda are necessary if you want to be able to *synthesize* the type of abstractions. If you switch to type *checking* as your modus operandi, you still need annotations to check a beta-redex like `(\ x -> t) s`, as you have no way to guess the types of the parts from that of the whole. I advise modern designers to check types and exclude beta-redexes from the very syntax.)

(*Digression*. This system is inconsistent as `Set:Set` allows the encoding of a variety of "liar paradoxes". When Martin-Löf proposed this theory, Girard sent him an encoding of it in his own inconsistent System U. The subsequent paradox due to Hurkens is the neatest toxic construction we know.)

### Combinator Syntax and Normalization ###

Anyhow, we have two extra symbols, Pi and Set, so we might perhaps manage a combinatory translation with S, K and two extra symbols: I chose U for the universe and P for the product.

Now we can define the untyped combinatory syntax (with free variables):

    data SKUP = S | K | U | P deriving (Show, Eq)

    data Unty a
      = C SKUP
      | Unty a :. Unty a
      | V a
      deriving (Functor, Eq)
    infixl 4 :.

Note that I've included the means to include free variables represented by type `a` in this syntax. Apart from being a reflex on my part (every syntax worthy of the name is a free monad with `return` embedding variables and `>>=` perfoming substitution), it'll be handy to represent intermediate stages in the process of converting terms with binding to their combinatory form.

Here's normalization:

    norm :: Unty a -> Unty a
    norm (f :. a)  = norm f $. a
    norm c         = c

    ($.) :: Unty a -> Unty a -> Unty a        -- requires first arg in normal form
    C S :. f :. a $. g  = f $. g $. (a :. g)  -- S f a g = f g (a g)   share environment
    C K :. a $. g       = a                   -- K a g = a             drop environment
    n $. g              = n :. norm g         -- guarantees output in normal form
    infixl 4 $.

(An exercise for the reader is to define a type for exactly the normal forms and sharpen the types of these operations.)

### Representing Type Theory ###

We can now define a syntax for our type theory.

    data Tm a
      = Var a
      | Lam (Tm a) (Tm (Su a))    -- Lam is the only place where binding happens
      | Tm a :$ Tm a
      | Pi (Tm a) (Tm a)          -- the second arg of Pi is a function computing a Set
      | Set
      deriving (Show, Functor)
    infixl 4 :$

    data Ze
    magic :: Ze -> a
    magic x = x `seq` error "Tragic!"

    data Su a = Ze | Su a deriving (Show, Functor, Eq)

I use a de Bruijn index representation in the Bellegarde and Hook manner (as popularised by Bird and Paterson). The type `Su a` has one more element than `a`, and we use it as the type of free variables under a binder, with `Ze` as the newly bound variable and `Su x` being the shifted representation of the old free variable `x`.

### Translating Terms to Combinators ###

And with that done, we acquire the usual translation, based on *bracket abstraction*.

    tm :: Tm a -> Unty a
    tm (Var a)    = V a
    tm (Lam _ b)  = bra (tm b)
    tm (f :$ a)   = tm f :. tm a
    tm (Pi a b)   = C P :. tm a :. tm b
    tm Set        = C U

    bra :: Unty (Su a) -> Unty a               -- binds a variable, building a function
    bra (V Ze)      = C S :. C K :. C K        -- the variable itself yields the identity
    bra (V (Su x))  = C K :. V x               -- free variables become constants
    bra (C c)       = C K :. C c               -- combinators become constant
    bra (f :. a)    = C S :. bra f :. bra a    -- S is exactly lifted application


### Typing the Combinators ###

The translation shows the way we use the combinators, which gives us quite a clue about what their types should be. `U` and `P` are just set constructors, so, writing untranslated types and allowing "Agda notation" for Pi, we should have

    U : Set
    P : (A : Set) -> (B : (a : A) -> Set) -> Set

The `K` combinator is used to lift a value of some type `A` to a constant function over some other type `G`.

      G : Set   A : Set
    -------------------------------
      K : (a : A) -> (g : G) -> A

The `S` combinator is used to lift applications over a type, upon which all of the parts may depend.

      G : Set
      A : (g : G) -> Set
      B : (g : G) -> (a : A g) -> Set
    ----------------------------------------------------
      S : (f : (g : G) ->    (a : A g) -> B g a   ) ->
          (a : (g : G) ->    A g                  ) ->
               (g : G) ->    B g (a g)

If you look at the type of `S`, you'll see that it exactly states the *contextualised* application rule of the type theory, so that's what makes it suitable to reflect the application construct. That's its job!

We then have application only for closed things

      f : Pi A B
      a : A
    --------------
      f a : B a

But there's a snag. I've written the types of the combinators in ordinary type theory, not combinatory type theory. Fortunately, I have a machine that will make the translation.


### A Combinatory Type System ###

    ---------
      U : U

    ---------------------------------------------------------
      P : PU(S(S(KP)(S(S(KP)(SKK))(S(KK)(KU))))(S(KK)(KU)))

      G : U
      A : U
    -----------------------------------------
      K : P[A](S(S(KP)(K[G]))(S(KK)(K[A])))

      G : U
      A : P[G](KU)
      B : P[G](S(S(KP)(S(K[A])(SKK)))(S(KK)(KU)))
    --------------------------------------------------------------------------------------
      S : P(P[G](S(S(KP)(S(K[A])(SKK)))(S(S(KS)(S(S(KS)(S(KK)(K[B])))(S(KK)(SKK))))
          (S(S(KS)(KK))(KK)))))(S(S(KP)(S(S(KP)(K[G]))(S(S(KS)(S(KK)(K[A])))
          (S(S(KS)(KK))(KK)))))(S(S(KS)(S(S(KS)(S(KK)(KP)))(S(KK)(K[G]))))
          (S(S(KS)(S(S(KS)(S(KK)(KS)))(S(S(KS)(S(S(KS)(S(KK)(KS)))
          (S(S(KS)(S(KK)(KK)))(S(KK)(K[B])))))(S(S(KS)(S(S(KS)(S(KK)(KS)))(S(KK)(KK))))
          (S(KK)(KK))))))(S(S(KS)(S(S(KS)(S(KK)(KS)))(S(S(KS)(S(KK)(KK)))
          (S(S(KS)(KK))(KK)))))(S(S(KS)(S(S(KS)(S(KK)(KS)))(S(KK)(KK))))(S(KK)(KK)))))))

      M : A   B : U
    ----------------- A ={norm} B
      M : B

So there you have it, in all its unreadable glory: a combinatory presentation of `Set:Set`!

There's still a bit of a problem. The syntax of the system gives you no way to guess the `G`, `A` and `B` parameters for `S` and similarly for `K`, just from the terms. Correspondingly, we can verify *typing derivations* algorithmically, but we can't just typecheck combinator terms as we could with the original system. What might work is to require the input to the typechecker to bear type annotations on uses of S and K, effectively recording the derivation. But that's another can of worms...

**This is a good place to stop, if you've been keen enough to start. The rest is "behind the scenes" stuff.**

### Generating the Types of the Combinators ###

I generated those combinatory types using the bracket abstraction translation from the relevant type theory terms. To show how I did it, and make this post not entirely pointless, let me offer my equipment.

I can write the types of the combinators, fully abstracted over their parameters, as follows. I make use of my handy `pil` function, which combines Pi and lambda to avoid repeating the domain type, and rather helpfully allows me to use Haskell's function space to bind variables. Perhaps you can almost read the following!

    pTy :: Tm a
    pTy = fmap magic $
      pil Set $ \ _A -> pil (pil _A $ \ _ -> Set) $ \ _B -> Set

    kTy :: Tm a
    kTy = fmap magic $
      pil Set $ \ _G -> pil Set $ \ _A -> pil _A $ \ a -> pil _G $ \ g -> _A

    sTy :: Tm a
    sTy = fmap magic $
      pil Set $ \ _G ->
      pil (pil _G $ \ g -> Set) $ \ _A ->
      pil (pil _G $ \ g -> pil (_A :$ g) $ \ _ -> Set) $ \ _B ->
      pil (pil _G $ \ g -> pil (_A :$ g) $ \ a -> _B :$ g :$ a) $ \ f ->
      pil (pil _G $ \ g -> _A :$ g) $ \ a ->
      pil _G $ \ g -> _B :$ g :$ (a :$ g)

With these defined, I extracted the relevant *open* subterms and ran them through the translation.

### A de Bruijn Encoding Toolkit ###

Here's how to build `pil`. Firstly, I define a class of `Fin`ite sets, used for variables. Every such set has a constructor-preserving `emb`edding into the set above, plus a new `top` element, and you can tell them apart: the `embd` function tells you if a value is in the image of `emb`.

    class Fin x where
      top :: Su x
      emb :: x -> Su x
      embd :: Su x -> Maybe x

We can, of course, instantiate `Fin` for `Ze` and `Suc`

    instance Fin Ze where
      top = Ze              -- Ze is the only, so the highest
      emb = magic
      embd _ = Nothing      -- there was nothing to embed

    instance Fin x => Fin (Su x) where
      top = Su top          -- the highest is one higher
      emb Ze     = Ze            -- emb preserves Ze
      emb (Su x) = Su (emb x)    -- and Su
      embd Ze      = Just Ze           -- Ze is definitely embedded
      embd (Su x)  = fmap Su (embd x)  -- otherwise, wait and see

Now I can define less-or-equals, with a *weakening* operation.

    class (Fin x, Fin y) => Le x y where
      wk :: x -> y

The `wk` function should embed the elements of `x` as the *largest* elements of `y`, so that the extra things in `y` are smaller, and thus in de Bruijn index terms, bound more locally.

    instance Fin y => Le Ze y where
      wk = magic    -- nothing to embed

    instance Le x y => Le (Su x) (Su y) where
      wk x = case embd x of
        Nothing  -> top          -- top maps to top
        Just y   -> emb (wk y)   -- embedded gets weakened and embedded

And once you've got that sorted out, a bit of rank-n skullduggery does the rest.

    lam :: forall x. Tm x -> ((forall y. Le (Su x) y => Tm y) -> Tm (Su x)) -> Tm x
    lam s f = Lam s (f (Var (wk (Ze :: Su x))))
    pil :: forall x. Tm x -> ((forall y . Le (Su x) y => Tm y) -> Tm (Su x)) -> Tm x
    pil s f = Pi s (lam s f)

The higher-order function doesn't just give you a term representing the variable, it gives you an *overloaded* thing which becomes the correct representation of the variable in any scope where the variable is visible. That is, the fact that I go to the trouble of distinguishing the different scopes *by type* gives the Haskell typechecker enough information to compute the shifting required for the translation to de Bruijn representation. Why keep a dog and bark yourself?
