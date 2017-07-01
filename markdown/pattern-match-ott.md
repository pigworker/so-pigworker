I guess I'll field this one. I find it a strange question, but that's because of my own particular journey. The short answer is: don't do pattern matching in OTT, or in any kernel type theory. Which is not the same thing as to not do pattern matching ever.

The long answer is basically my PhD thesis.

In my PhD thesis, I show how to elaborate high-level programs written in a pattern matching style into a kernel type theory which has only the induction principles for inductive datatypes and a suitable treatment of propositional equality. The elaboration of pattern matching introduces propositional equations on datatype indices, then solves them by unification. Back then, I was using an intensional equality, but observational equality gives you at least the same power. That is: my technology for elaborating pattern matching (and thus keeping it out of the kernel theory), hiding all the equational piggery-jokery, predates the upgrade to observational equality. The ghastly vlookup you've used to illustrate your point might correspond to the output of the elaboration process, but the input need not be that bad. The nice definition

    vlookup : Fin n -> Vec X n -> X
    vlookup fz     (vcons x xs) = x
    vlookup (fs i) (vcons x xs) = vlookup i xs

elaborates just fine. The equation-solving that happens along the way is just the same equation-solving that Agda does at the meta-level when checking a definition by pattern matching, or that Haskell does. Don't be fooled by programs like

    f :: a ~ b => a -> b
    f x = x

In *kernel* Haskell, that elaborates to some sort of

    f {q} x = coerce q x

but it's not in your face. And it's not in compiled code, either. OTT equality proofs, like Haskell equality proofs, can be erased before computing with *closed* terms.

*Digression.* To be clear about the status of equality data in Haskell, the GADT

    data Eq :: k -> k -> * where
      Refl :: Eq x x

really gives you

    Refl :: x ~ y -> Eq x y

but because the type system is not logically sound, type safety relies on strict pattern matching on that type: you can't erase `Refl` and you really must compute it and match it at run time, but you *can* erase the data corresponding to the proof of `x~y`. In OTT, the entire propositional fragment is proof-irrelevant for open terms and erasable for closed computation. *End of digression.*

The decidability of equality on this or that datatype is not especially relevant (at least, not if you have uniqueness of identity proofs; if you don't always have UIP, decidability is one way to get it sometimes). The equational problems which show up in pattern matching are on arbitrary *open* expressions. That's a lot of rope. But a machine can certainly decide the fragment which consists of first-order expressions built from variables and fully applied constructors (and that's what Agda does when you split cases: if the constraints are too weird, the thing just barfs). OTT should allow us to push a bit further into the decidable fragments of higher-order unification. If you know `(forall x. f x = t[x])` for unknown `f`, that's equivalent to `f = \ x -> t[x]`.

So, "no pattern matching in OTT" has always been a deliberate design choice, as we always intended it to be an elaboration target for a translation we already knew how to do. Rather, it's a strict upgrade in kernel theory power.
