Firstly, thanks for asking about Observational Type Theory. Secondly, what you've done here *does* seem to hang together, even though it has things in different places from where Thorsten Altenkirch, Wouter Swierstra and I put them in our version of the story. Thirdly, it's no surprise (at least not to me) that coherence is derivable, leaving reflexivity the only postulate. That's true of our OTT as well, and Wouter did the proofs in Agda 1, back when we wrote that paper. Proof irrelevance and the shortness of life meant I didn't port his proofs to Agda 2.

If you've missed anything, it's lurking in your remark

> We still need to postulate something to define `subst` and other stuff.

If you have some `P : X -> Set`, some `a, b : X` and some `q : a = b`, you expect to get a function in `P a -> P b`. The "equal functions take equal inputs to equal outputs" formulation gives you that, as `refl P : P = P`, so from `q`, we can deduce `P a = P b`. Your "equal functions take a given input to equal outputs" formulation does not allow you to let `q` bridge the gap from `a` to `b`.

In the presence of `refl` and `subst`, "two equal inputs" amounts to the same thing as "one input used in two places". It seems to me that you've moved the work into whatever else you need to get `subst`. Depending on how lazy your definition of `coerce` is (and *that*'s how you get proof irrelevance), you will need only a postulate.

With your particular formulation, you might even get away with a *homogeneous* value equality. If you're fixing type gaps with coercions rather than equations, you might save yourself some trouble (and maybe get rid of that equation on the domain type in function equality). Of course, in that case, you'd need to think about how to replace the statement of coherence.

We tried quite hard to keep coercion out of the definition of equality, to retain some sort of symmetry, and to keep type equations out of value equations, mostly to have less to think about at one go. It's interesting to see that at least some parts of the construction might get easier with "a thing and its coercion" replacing "two equal things".
