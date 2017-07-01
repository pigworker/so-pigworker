We've had some excellent answers, but as the perpetrator, I thought I'd offer some remarks.

Yes, there are multiple equivalent presentations of these lemmas. The presentation I use is one of them, and the choice is largely a pragmatic one. These days (in a more recent codebase), I go as far as to define

    -- Holds :: Constraint -> *
    type Holds c = forall t . (c => t) -> t

This is an example of an *eliminator type*: it abstracts over what it delivers (the *motive* of the elimination) and it requires you to construct zero or more *methods* (one, here) of achieving the motive under more specific circumstances. The way to read it is *backwards*. It says

> If you have a problem (to inhabit any motive type `t`), and nobody else can help, maybe you can make progress by assuming constraint `c` in your method.

Given that the language of constraints admits conjunction (aka tupling), we acquire the means to write lemmas of the form

    lemma :: forall x1 .. xn. (p1[x1 .. xn],.. pm[x1 .. xn])        -- premises
                           => t1[x1 .. xn] -> .. tl[x1 .. xn]       -- targets
                           -> Holds (c1[x1 .. xn],.. ck[x1 .. xn])  -- conclusions

and it might even be that some constraint, a premise `p` or a conclusion `c`, has the form of an equation

    l[x1 .. xn] ~ r[x1 .. cn]

Now, to deploy such a `lemma`, consider the problem of filling a hole

    _ :: Problem

Refine this `_` by the elimination `lemma`, specifying the *targets*. The *motive* comes from the problem at hand. The *method* (singular in the case of `Holds`) remains open.

    lemma target1 .. targetl $ _

and the method hole will not have changed type

    _ :: Problem

but GHC will know a bunch more stuff and thus be more likely to believe your solution.

Sometimes, there's a constraint-versus-data choice to make for what's a (constraint) premise and what's a (data) target. I tend to pick these to avoid ambiguity (Simon likes to guess the `x1 .. xn`, but sometimes needs a hint) and to facilitate *proof by induction*, which is much easier on targets (often the singletons for type level data) than on premises.

As to deployment, for equations, you can certainly switch to a datatype presentation and break out a case analysis

    case dataLemma target1 .. targetl of Refl -> method

and indeed, if you equip yourself with the `Dict` existential

    data Dict (c :: Constraint) :: * where
      Dict :: c => Dict c

you can do a bunch at once

    case multiLemma blah blah blah of (Refl, Dict, Dict, Refl) -> method

but the eliminator form is more compact and readable *when there is at most one method*. Indeed, we can chain multiple lemmas without sliding ever rightward

    lemma1 ..   $
    ...
    lemmaj ..   $
    method

If you have such an eliminator with two or more cases, I think it's often better to wrap it up as a GADT, so that usage sites helpfully tag each case with a constructor label.

Anyhow, yes, the point is to choose the presentation of the facts which most compactly enables us to extend the reach of GHC's constraint solving machinery so that more stuff just typechecks. If you're in a scrap with Simon, it's often a good strategy to explain yourself to Dimitrios next door.
