Given that every `Applicative` has a `Backwards` counterpart,

    newtype Backwards f x = Backwards {backwards :: f x}
    instance Applicative f => Applicative (Backwards f) where
      pure x = Backwards (pure x)
      Backwards ff <*> Backwards fs = Backwards (flip ($) <$> fs <*> ff)

it's *unusual* for `Applicative` to be uniquely determined, just as (and this is very far from unrelated) many sets extend to monoids in multiple ways.

In [this answer][1], I set the exercise of finding at least four distinct valid `Applicative` instances for nonempty lists: I won't spoil it here, but I will give a big hint on how to hunt.

Meanwhile, in some wonderful recent work (which I saw at a summer school a few months ago), Tarmo Uustalu showed a rather neat way to get a handle on this problem, at least when the underlying functor is a *container*, in the sense of Abbott, Altenkirch and Ghani.

**Warning: Dependent types ahead!**

**What is a container?** If you have dependent types to hand, you can present container-like functors F uniformly, as being determined by two components

  1. a set of shapes, S : Set
  2. an S-indexed set of positions, P : S -> Set

Up to isomorphism, container data structures in F X are given by the dependent pair of some shape s : S, and some function e : P s -> X, which tells you the element located at each position. That is, we define the extension of a container

    (S <| P) X = (s : S) * (P s -> X)

(which, by the way, looks a lot like a generalized power series if you read `->` as reversed exponentiation). The triangle is supposed to remind you of a tree node sideways, with an element s : S labelling the apex, and the baseline representing the position set P s. We say that some functor is a container if it is isomorphic to some `S <| P`.

In Haskell, you can easily take `S = F ()`, but constructing `P` can take quite a bit of type-hackery. But that *is* something you can try at home. You'll find that containers are closed under all the usual polynomial type-forming operations, as well as identity,

    Id ~= () <| \ _ -> ()

composition, where a whole shape is made from just one outer shape and an inner shape for each outer position,

    (S0 <| P0) . (S1 <| P1)  ~=  ((S0 <| P0) S1) <| \ (s0, e0) -> (p0 : P0, P1 (e0 p0))

and some other things, notably the *tensor*, where there is one outer and one inner shape (so "outer" and "inner" are interchangeable)

    (S0 <| P0) (X) (S1 <| P1)   =   ((S0, S1) <| \ (s0, s1) -> (P0 s0, P1 s1))

so that `F (X) G` means "`F`-structures of `G`-structures-all-the-same-shape", e.g., `[] (X) []` means *rectangular* lists-of-lists. But I digress

**Polymorphic functions between containers** Every polymorphic function

    m : forall X. (S0 <| P0) X -> (S1 <| P1) X

can be implemented by a *container morphism*, constructed from two components in a very particular way.

 1. a function `f : S0 -> S1` mapping input shapes to output shapes;
 2. a function `g : (s0 : S0) -> P1 (f s0) -> P0 s0` mapping output positions to input positions.

Our polymorphic function is then

    \ (s0, e0) -> (f s0, e0 . g s0)

where the output shape is computed from the input shape, then the output positions are filled up by picking elements from input positions.

(If you're Peter Hancock, you have a whole other metaphor for what's going on. Shapes are Commands; Positions are Responses; a container morphism is a *device driver*, translating commands one way, then responses the other.)

Every container morphism gives you a polymorphic function, but the reverse is also true. Given such an m, we may take

    (f s, g s) = m (s, id)

That is, we have a *representation theorem*, saying that every polymorphic function between two containers is given by such an `f`, `g`-pair.

**What about `Applicative`?** We kind of got a bit lost along the way, building all this machinery. But it *has* been worth it. When the underlying functors for monads and applicatives are containers, the polymorphic functions `pure` and `<*>`, `return` and `join` must be representable by the relevant notion of container morphism.

Let's take applicatives first, using their monoidal presentation. We need

    unit : () -> (S <| P) ()
    mult : forall X, Y. ((S <| P) X, (S <| P) Y) -> (S <| P) (X, Y)

The left-to-right maps for shapes require us to deliver

    unitS : () -> S
    multS : (S, S) -> S

so it looks like we might need a monoid. And when you check that the applicative laws, you find we need *exactly* a monoid. Equipping a container with applicative structure is *exactly* refining the monoid structures on its shapes with suitable position-respecting operations. There's nothing to do for `unit` (because there is no chocie of source position), but for `mult`, we need that whenenver

    multS (s0, s1) = s

we have

    multP (s0, s1) : P s -> (P s0, P s1)

satisfying appropriate identity and associativity conditions. If we switch to Hancock's interpretation, we're defining a monoid (skip, semicolon) for commands, where there is no way to look at the response to the first command before choosing the second, like commands are a deck of punch cards. We have to be able to chop up responses to combined commands into the individual responses to the individual commands.

So, every monoid on the shapes gives us a potential applicative structure. For lists, shapes are numbers (lengths), and there are a great many monoids from which to choose. Even if shapes live in `Bool`, we have quite a bit of choice.

**What about `Monad`?** Meanwhile, for monads `M` with `M ~= S <| P`. We need 

    return : Id -> M
    join   : M . M -> M

Looking at shapes first, that means we need a sort-of lopsided monoid.

    return_f : () -> S
    join_f   : (S <| P) S -> S  --  (s : S, P s -> S) -> S

It's lopsided because we get a bunch of shapes on the right, not just one. If we switch to Hancock's interpretation, we're defining a kind of sequential composition for commands, where we do let the second command be chosen on the basis of the first response, like we're interacting at a teletype. More geometrically, we're explaining how to glom two layers of a tree into one. It would be very surprising if such compositions were unique.

Again, for the positions, we have to map single output positions to pairs in a coherent way. This is trickier for monads: we first choose an outer position (response), then we have to choose an inner position(response) appropriate to the shape (command) found at the first position (chosen after the first response).

I'd love to link to Tarmo's work for the details, but it doesn't seem to have hit the streets yet. He has actually used this analysis to enumerate all possible monad structures for several choices of underlying container. I'm looking forward to the paper!

**Edit.** By way of doing honour to the other answer, I should observe that when everywhere `P s = ()`, then `(S <| P) X ~= (S, X)` and the monad/applicative structures coincide exactly with each other and with the monoid structures on `S`. That is, for writer monads, we need only choose the shape-level operations, because there is exactly one position for a value in every case.

  [1]: https://stackoverflow.com/a/32825891/828361
