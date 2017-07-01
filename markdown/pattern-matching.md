Here's a sketch of an algorithm. It's also the basis of Lennart Augustsson's celebrated technique for compiling pattern matching efficiently. (The [paper](http://www.springerlink.com/content/y647423656557505/) is in that incredible FPCA proceedings (LNCS 201) with oh so many hits.) The idea is to reconstruct an exhaustive, non-redundant analysis by repeatedly splitting the most general pattern into constructor cases.

In general, the problem is that your program has a possibly empty bunch of &lsquo;actual&rsquo; patterns {p<sub>1</sub>, .., p<sub>n</sub>}, and you want to know if they cover a given &lsquo;ideal&rsquo; pattern q. To kick off, take q to be a variable x. The invariant, initially satisfied and subsequently maintained, is that each p<sub>i</sub> is &sigma;<sub>i</sub>q for some substitution &sigma;<sub>i</sub> mapping variables to patterns.

How to proceed. If n=0, the bunch is empty, so you have a possible case q that isn't covered by a pattern. Complain that the ps are not exhaustive. If &sigma;<sub>1</sub> is an injective <i>renaming</i> of variables, then p<sub>1</sub> catches every case that matches q, so we're warm: if n=1, we win; if n>1 then oops, there's no way p<sub>2</sub> can ever be needed. Otherwise, we have that for some variable x, &sigma;<sub>1</sub>x is a constructor pattern. In that case split the problem into multiple subproblems, one for each constructor c<sub>j</sub> of x's type. That is, split the original q into multiple ideal patterns q<sub>j</sub> = [x:=c<sub>j</sub> y<sub>1</sub> .. y<sub>arity(c<sub>j</sub>)</sub>]q, and refine the patterns accordingly for each q<sub>j</sub> to maintain the invariant, dropping those that don't match.

Let's take the example with `{[], x :: y :: zs}` (using `::` for `cons`). We start with

      xs covering  {[], x :: y :: zs}

and we have [xs := []] making the first pattern an instance of the ideal. So we split xs, getting

      [] covering {[]}
      x :: ys covering {x :: y :: zs}

The first of these is justified by the empty injective renaming, so is ok. The second takes [x := x, ys := y :: zs], so we're away again, splitting ys, getting.

      x :: [] covering {}
      x :: y :: zs covering {x :: y :: zs}

and we can see from the first subproblem that we're banjaxed.

The overlap case is more subtle and allows for variations, depending on whether you want to flag up any overlap, or just patterns which are completely redundant in a top-to-bottom priority order. Your basic rock'n'roll is the same. E.g., start with

      xs covering {[], ys}

with [xs := []] justifying the first of those, so split. Note that we have to refine ys with constructor cases to maintain the invariant.

      [] covering {[], []}
      x :: xs covering {y :: ys}

Clearly, the first case is strictly an overlap. On the other hand, when we notice that refining an actual program pattern is needed to maintain the invariant, we can filter out those strict refinements that become redundant and check that at least one survives (as happens in the `::` case here).

So, the algorithm builds a set of ideal exhaustive overlapping patterns q in a way that's motivated by the actual program patterns p. You split the ideal patterns into constructor cases whenever the actual patterns demand more detail of a particular variable. If you're lucky, each actual pattern is covered by disjoint nonempty sets of ideal patterns and each ideal pattern is covered by just one actual pattern. The tree of case splits which yield the ideal patterns gives you the efficient jump-table driven compilation of the actual patterns.

The algorithm I've presented is clearly terminating, but if there are datatypes with no constructors, it can fail to accept that the empty set of patterns is exhaustive. This is a serious issue in dependently typed languages, where exhaustiveness of conventional patterns is undecidable: the sensible approach is to allow "refutations" as well as equations. In Agda, you can write (), pronounced "my Aunt Fanny", in any place where no constructor refinement is possible, and that absolves you from the requirement to complete the equation with a return value. Every exhaustive set of patterns can be made *recognizably* exhaustive by adding in enough refutations.

Anyhow, that's the basic picture.
