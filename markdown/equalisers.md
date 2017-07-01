**tl;dr the proposed candidate is not quite an equaliser, but its irrelevant counterpart is**

The candidate for an equaliser in Agda looks good. So let's just try it. We'll need some basic kit. Here are my refusenik ASCII dependent pair type and homogeneous intensional equality.

    record Sg (S : Set)(T : S -> Set) : Set where
      constructor _,_
      field
        fst : S
        snd : T fst
    open Sg

    data _==_ {X : Set}(x : X) : X -> Set where
      refl : x == x

Here's your candidate for an equaliser for two functions

    Q : {S T : Set}(f g : S -> T) -> Set
    Q {S}{T} f g = Sg S \ s -> f s == g s

with the `fst` projection sending `Q f g` into `S`.

What it says: an element of `Q f g` is an element `s` of the source type, together with a proof that `f s == g s`. But is this an equaliser? Let's try to make it so.

To say what an equaliser is, I should define function composition.

    _o_ : {R S T : Set} -> (S -> T) -> (R -> S) -> R -> T
    (f o g) x = f (g x)

So now I need to show that any `h : R -> S` which identifies `f o h` and `g o h` must factor through the candidate `fst : Q f g -> S`. I need to deliver both the other component, `u : R -> Q f g` and the proof that indeed `h` factors as `fst o u`. Here's the picture: `(Q f g , fst)` is an equalizer if whenever the diagram commutes without `u`, there is a unique way to add `u` with the diagram still commuting.

![equaliser diagram][1]

Here goes existence of the mediating `u`.

    mediator : {R S T : Set}(f g : S -> T)(h : R -> S) ->
               (q : (f o h) == (g o h)) ->
               Sg (R -> Q f g) \ u -> h == (fst o u)

Clearly, I should pick the same element of `S` that `h` picks.

    mediator f g h q = (\ r -> (h r , ?0)) , ?1

leaving me with two proof obligations

    ?0 : f (h r) == g (h r)
    ?1 : h == (\ r -> h r)

Now, `?1` can just be `refl` as Agda's definitional equality has the eta-law for functions. For `?0`, we are blessed by `q`. Equal functions respect application

    funq : {S T : Set}{f g : S -> T} -> f == g -> (s : S) -> f s == g s
    funq refl s = refl

so we may take `?0 = funq q r`.

But let us not celebrate prematurely, for the existence of a mediating morphism is not sufficient. We require also its uniqueness. And here the wheel is likely to go wonky, because `==` is *intensional*, so uniqueness means there's only ever one way to *implement* the mediating map. But then, our assumptions are also intensional...

Here's our proof obligation. We must show that any other mediating morphism is equal to the one chosen by `mediator`.

    mediatorUnique :
      {R S T : Set}(f g : S -> T)(h : R -> S) ->
      (qh : (f o h) == (g o h)) ->
      (m : R -> Q f g) ->
      (qm : h == (fst o m)) ->
      m == fst (mediator f g h qh)

We can immediately substitute via `qm` and get

    mediatorUnique f g .(fst o m) qh m refl = ?

    ? :  m == (\ r -> (fst (m r) , funq qh r))

which looks good, because Agda has eta laws for records, so we know that

    m == (\ r -> (fst (m r) , snd (m r)))

but when we try to make `? = refl`, we get the complaint

    snd (m _) != funq qh _ of type f (fst (m _)) == g (fst (m _))

which is annoying, because identity proofs are unique (in the standard configuration). Now, you can get out of this by postulating extensionality and using a few other facts about equality

    postulate ext : {S T : Set}{f g : S -> T} -> ((s : S) -> f s == g s) -> f == g

    sndq : {S : Set}{T : S -> Set}{s : S}{t t' : T s} ->
           t == t' -> _==_ {Sg S T} (s , t) (s , t')
    sndq refl = refl

    uip : {X : Set}{x y : X}{q q' : x == y} -> q == q'
    uip {q = refl}{q' = refl} = refl

    ? = ext (\ s -> sndq uip)

but that's overkill, because the only problem is the annoying equality proof mismatch: the computable parts of the implementations match on the nose. So the fix is to work with *irrelevance*. I replace `Sg` by the `Ex`istential quantifier, whose second component is marked as irrelevant with a dot. Now it matters not which proof we use that the witness is good.

    record Ex (S : Set)(T : S -> Set) : Set where
      constructor _,_
      field
        fst : S
        .snd : T fst
    open Ex

and the new candidate equaliser is

    Q : {S T : Set}(f g : S -> T) -> Set
    Q {S}{T} f g = Ex S \ s -> f s == g s

The entire construction goes through as before, except that in the last obligation

    ? = refl

is accepted!

So yes, even in the intensional setting, eta laws and the ability to mark fields as irrelevant give us equalisers.

**No undecidable typechecking was involved in this construction.**


  [1]: http://i.stack.imgur.com/odrtv.jpg
