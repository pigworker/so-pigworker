### Dependently Typed Haskell, Now? ###

Haskell is, to a small extent, a dependently typed language. There is a notion of type-level data, now more sensibly typed thanks to `DataKinds`, and there is some means (`GADTs`) to give a run-time
representation to type-level data. Hence, **values of run-time stuff effectively show up in types**, which is what it means for a language to be dependently typed.

Simple datatypes are *promoted* to the kind level, so that the values
they contain can be used in types. Hence the archetypal example

    data Nat = Z | S Nat

    data Vec :: Nat -> * -> * where
      VNil   :: Vec Z x
      VCons  :: x -> Vec n x -> Vec (S n) x

becomes possible, and with it, definitions such as

    vApply :: Vec n (s -> t) -> Vec n s -> Vec n t
    vApply VNil         VNil         = VNil
    vApply (VCons f fs) (VCons s ss) = VCons (f s) (vApply fs ss)


which is nice. Note that the length `n` is a purely static thing in
that function, ensuring that the input and output vectors have the
same length, even though that length plays no role in the execution of
`vApply`. By contrast, it's much trickier (i.e., impossible) to
implement the function which makes `n` copies of a given `x` (which
would be `pure` to `vApply`'s `<*>`)

    vReplicate :: x -> Vec n x

because it's vital to know how many copies to make at run-time. Enter
singletons.

    data Natty :: Nat -> * where
      Zy :: Natty Z
      Sy :: Natty n -> Natty (S n)

For any promotable type, we can build the singleton family, indexed
over the promoted type, inhabited by run-time duplicates of its
values. `Natty n` is the type of run-time copies of the type-level `n
:: Nat`. We can now write

    vReplicate :: Natty n -> x -> Vec n x
    vReplicate Zy     x = VNil
    vReplicate (Sy n) x = VCons x (vReplicate n x)

So there you have a type-level value yoked to a run-time value:
inspecting the run-time copy refines static knowledge of the
type-level value. Even though terms and types are separated, we can
work in a dependently typed way by using the singleton construction as
a kind of epoxy resin, creating bonds between the phases. That's a
long way from allowing arbitrary run-time expressions in types, but it ain't nothing.


### What's Nasty? What's Missing? ###

Let's put a bit of pressure on this technology and see what starts
wobbling. We might get the idea that singletons should be manageable a
bit more implicitly

    class Nattily (n :: Nat) where
      natty :: Natty n
    instance Nattily Z where
      natty = Zy
    instance Nattily n => Nattily (S n) where
      natty = Sy natty

allowing us to write, say,

    instance Nattily n => Applicative (Vec n) where
      pure = vReplicate natty
      (<*>) = vApply

That works, but it now means that our original `Nat` type has spawned
three copies: a kind, a singleton family and a singleton class. We
have a rather clunky process for exchanging explicit `Natty n` values
and `Nattily n` dictionaries. Moreover, `Natty` is not `Nat`: we have
some sort of dependency on run-time values, but not at the type we
first thought of. No fully dependently typed language makes dependent
types this complicated!

Meanwhile, although `Nat` can be promoted, `Vec` cannot. You can't
index by an indexed type. Full on dependently typed languages impose
no such restriction, and in my career as a dependently typed show-off,
I've learned to include examples of two-layer indexing in my talks,
just to teach folks who've made one-layer indexing
difficult-but-possible not to expect me to fold up like a house of
cards. What's the problem? Equality. GADTs work by translating the
constraints you achieve implicitly when you give a constructor a
specific return type into explicit equational demands. Like this.

    data Vec (n :: Nat) (x :: *)
      = n ~ Z => VNil
      | forall m. n ~ S m => VCons x (Vec m x)

In each of our two equations, both sides have kind `Nat`.

Now try the same translation for something indexed over vectors.

    data InVec :: x -> Vec n x -> * where
      Here :: InVec z (VCons z zs)
      After :: InVec z ys -> InVec z (VCons y ys)

becomes

    data InVec (a :: x) (as :: Vec n x)
      = forall m z (zs :: Vec x m). (n ~ S m, as ~ VCons z zs) => Here
      | forall m y z (ys :: Vec x m). (n ~ S m, as ~ VCons y ys) => After (InVec z ys)

and now we form equational constraints between `as :: Vec n x` and
`VCons z zs :: Vec (S m) x` where the two sides have syntactically
distinct (but provably equal) kinds. GHC core is not currently
equipped for such a concept!

What else is missing? Well, **most of Haskell** is missing from the type
level. The language of terms which you can promote has just variables
and non-GADT constructors, really. Once you have those, the `type family` machinery allows you to write type-level programs: some of
those might be quite like functions you would consider writing at the
term level (e.g., equipping `Nat` with addition, so you can give a
good type to append for `Vec`), but that's just a coincidence!

Another thing missing, in practice, is a *library* which makes
use of our new abilities to index types by values. What do `Functor`
and `Monad` become in this brave new world? I'm thinking about it, but
there's a lot still to do.


### Running Type-Level Programs ###

Haskell, like most dependently typed programming languages, has *two*
operational semanticses. There's the way the run-time system runs
programs (closed expressions only, after type erasure, highly
optimised) and then there's the way the typechecker runs programs
(your type families, your "type class Prolog", with open expressions). For Haskell, you don't normally mix
the two up, because the programs being executed are in different
languages. Dependently typed languages have separate run-time and
static execution models for the *same* language of programs, but don't
worry, the run-time model still lets you do type erasure and, indeed,
proof erasure: that's what Coq's *extraction* mechanism gives you;
that's at least what Edwin Brady's compiler does (although Edwin
erases unnecessarily duplicated values, as well as types and
proofs). The phase distinction may not be a distinction of *syntactic category*
any longer, but it's alive and well.

Dependently typed languages, being total, allow the typechecker to run
programs free from the fear of anything worse than a long wait. As
Haskell becomes more dependently typed, we face the question of what
its static execution model should be? One approach might be to
restrict static execution to total functions, which would allow us the
same freedom to run, but might force us to make distinctions (at least
for type-level code) between data and codata, so that we can tell
whether to enforce termination or productivity. But that's not the only
approach. We are free to choose a much weaker execution model which is
reluctant to run programs, at the cost of making fewer equations come
out just by computation. And in effect, that's what GHC actually
does. The typing rules for GHC core make no mention of *running*
programs, but only for checking evidence for equations. When
translating to the core, GHC's constraint solver tries to run your type-level programs,
generating a little silvery trail of evidence that a given expression
equals its normal form. This evidence-generation method is a little
unpredictable and inevitably incomplete: it fights shy of
scary-looking recursion, for example, and that's probably wise. One
thing we don't need to worry about is the execution of `IO`
computations in the typechecker: remember that the typechecker doesn't have to give
`launchMissiles` the same meaning that the run-time system does!


### Hindley-Milner Culture ###

The Hindley-Milner type system achieves the truly awesome coincidence
of four distinct distinctions, with the unfortunate cultural
side-effect that many people cannot see the distinction between the
distinctions and assume the coincidence is inevitable! What am I
talking about?

 - terms *vs* types
 - explicitly written things *vs* implicitly written things
 - presence at run-time *vs* erasure before run-time
 - non-dependent abstraction *vs* dependent quantification

We're used to writing terms and leaving types to be inferred...and
then erased. We're used to quantifying over type variables with the
corresponding type abstraction and application happening silently and
statically.

You don't have to veer too far from vanilla Hindley-Milner
before these distinctions come out of alignment, and that's *no bad thing*. For a start, we can have more interesting types if we're willing to write them in a few
places. Meanwhile, we don't have to write type class dictionaries when
we use overloaded functions, but those dictionaries are certainly
present (or inlined) at run-time. In dependently typed languages, we
expect to erase more than just types at run-time, but (as with type
classes) that some implicitly inferred values will not be
erased. E.g., `vReplicate`'s numeric argument is often inferable from the type of the desired vector, but we still need to know it at run-time.

Which language design choices should we review because these
coincidences no longer hold? E.g., is it right that Haskell provides
no way to instantiate a `forall x. t` quantifier explicitly? If the
typechecker can't guess `x` by unifiying `t`, we have no other way to
say what `x` must be.

More broadly, we cannot treat "type inference" as a monolithic concept
that we have either all or nothing of. For a start, we need to split
off the "generalisation" aspect (Milner's "let" rule), which relies heavily on
restricting which types exist to ensure that a stupid machine can
guess one, from the "specialisation" aspect (Milner's "var" rule)
which is as effective as your constraint solver. We can expect that
top-level types will become harder to infer, but that internal type
information will remain fairly easy to propagate.


### Next Steps For Haskell ###

We're seeing the type and kind levels grow very similar (and they
already share an internal representation in GHC). We might as well
merge them. It would be fun to take `* :: *` if we can: we lost
*logical* soundness long ago, when we allowed bottom, but *type*
soundness is usually a weaker requirement. We must check. If we must have
distinct type, kind, etc levels, we can at least make sure everything
at the type level and above can always be promoted. It would be great
just to re-use the polymorphism we already have for types, rather than
re-inventing polymorphism at the kind level.

We should simplify and generalise the current system of constraints by
allowing *heterogeneous* equations `a ~ b` where the kinds of `a` and
`b` are not syntactically identical (but can be proven equal). It's an
old technique (in my thesis, last century) which makes dependency much
easier to cope with. We'd be able to express constraints on
expressions in GADTs, and thus relax restrictions on what can be
promoted.

We should eliminate the need for the singleton construction by
introducing a dependent function type, `pi x :: s -> t`. A function
with such a type could be applied *explicitly* to any expression of type `s` which
lives in the *intersection* of the type and term languages (so,
variables, constructors, with more to come later). The corresponding
lambda and application would not be erased at run-time, so we'd be
able to write

    vReplicate :: pi n :: Nat -> x -> Vec n x
    vReplicate Z     x = VNil
    vReplicate (S n) x = VCons x (vReplicate n x)

without replacing `Nat` by `Natty`. The domain of `pi` can be any
promotable type, so if GADTs can be promoted, we can write dependent
quantifier sequences (or "telescopes" as de Briuijn called them)

    pi n :: Nat -> pi xs :: Vec n x -> ...

to whatever length we need.

The point of these steps is to *eliminate complexity* by working directly with more general tools, instead of making do with weak tools and clunky encodings. The current partial buy-in makes the benefits of Haskell's sort-of dependent types more expensive than they need to be.


### Too Hard? ###

Dependent types make a lot of people nervous. They make me nervous,
but I like being nervous, or at least I find it hard not to be nervous
anyway. But it doesn't help that there's quite such a fog of ignorance
around the topic. Some of that's due to the fact that we all still
have a lot to learn. But proponents of less radical approaches have
been known to stoke fear of dependent types without always making sure
the facts are wholly with them. I won't name names. These "undecidable typechecking", "Turing incomplete", "no phase distinction", "no type erasure", "proofs everywhere", etc, myths persist, even though they're rubbish.

It's certainly not the case that dependently typed programs must
always be proven correct. One can improve the basic hygiene of one's
programs, enforcing additional invariants in types without going all
the way to a full specification. Small steps in this direction quite
often result in much stronger guarantees with few or no additional
proof obligations. It is not true that dependently typed programs are
inevitably *full* of proofs, indeed I usually take the presence of any
proofs in my code as the cue to *question my definitions*.

For, as with any increase in articulacy, we become free to say foul
new things as well as fair. E.g., there are plenty of crummy ways to
define binary search trees, but that doesn't mean there isn't [a good way][1]. It's important not to presume that bad experiences cannot be
bettered, even if it dents the ego to admit it. Design of dependent
definitions is a new skill which takes learning, and being a Haskell
programmer does not automatically make you an expert! And even if some
programs are foul, why would you deny others the freedom to be fair?


### Why Still Bother With Haskell? ###

I really enjoy dependent types, but most of my hacking projects are
still in Haskell. Why? Haskell has type classes. Haskell has useful
libraries. Haskell has a workable (although far from ideal) treatment
of programming with effects. Haskell has an industrial strength
compiler. The dependently typed languages are at a much earlier stage
in growing community and infrastructure, but we'll get there, with a
real generational shift in what's possible, e.g., by way of
metaprogramming and datatype generics. But you just have to look
around at what people are doing as a result of Haskell's steps towards
dependent types to see that there's a lot of benefit to be gained by
pushing the present generation of languages forwards, too.


  [1]: https://stackoverflow.com/a/10659438/828361
