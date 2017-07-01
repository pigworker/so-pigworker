It's really odd that people think pattern matching on types is bad. We get a lot of mileage out of pattern matching on data which *encode* types, whenever we do a universe construction. If you take the approach that Thorsten Altenkirch and I pioneered (and which my comrades and I began to engineer), the types do form a *closed* universe, so you don't even need to solve the (frankly worth solving) problem of computing with open datatypes to treat types as data. If we could pattern match on types directly, we wouldn't need a decoding function to map type codes to their meanings, which at worst reduces clutter, and at best reduces the need to prove and coerce by equational laws about the behaviour of the decoding function. I have every intention of building a no-middleman closed type theory this way. Of course, you need that level 0 types inhabit a level 1 datatype. That happens as a matter of course when you build an inductive-recursive universe hierarchy.

But what about parametricity, I hear you ask?

Firstly, I don't want parametricity when I'm trying to write type-generic code. Don't force parametricity on me.

Secondly, why should types be the only things we're parametric in? Why shouldn't we *sometimes* be parametric in other stuff, e.g., perfectly ordinary type indices which inhabit datatypes but which we'd prefer not to have at run time? It's a real nuisance that quantities which play a part only in *specification* are, just because of their type, forced to be present.

**The type of a domain has *nothing whatsoever* to do with whether quantification over it should be parametric.**

Let's have (e.g. as proposed by Bernardy and friends) a discipline where both parametric/erasable and non-parametric/matchable quantification are distinct and both available. Then types can be data and we can still say what we mean.
