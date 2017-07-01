Late to the party, this answer is basically a shameless plug.

Sam Lindley and I wrote a paper about [Hasochism][1], the pleasure and pain of dependently typed programming in Haskell. It gives plenty of examples of what's possible *now* in Haskell and draws points of comparison (favourable as well as not) with the Agda/Idris generation of dependently typed languages.

Although it is an academic paper, it is about actual programs, and you can grab the code from [Sam's repo][2]. We have lots of little examples (e.g. orderedness of mergesort output) but we end up with a text editor example, where we use indexing by width and height to manage screen geometry: we make sure that components are regular rectangles (vectors of vectors, not ragged lists of lists) and that they fit together exactly.

The key power of dependent types is to maintain consistency between separate data components (e.g., the head vector in a matrix and every vector in its tail must all have the same length). That's never more important than when writing conditional code. The situation (which will one day come to be seen as having been ridiculously naïve) is that the following are all type-preserving rewrites

  * `if b then t else e` => `if b then e else t`
  * `if b then t else e` => `t`
  * `if b then t else e` => `e`

Although we are presumably testing `b` because it gives us some useful insight into what would be appropriate (or even safe) to do next, none of that insight is mediated via the type system: the idea that `b`'s truth justifies `t` and its falsity justifies `e` is missing, despite being critical.

Plain old Hindley-Milner does give us one means to ensure some consistency. Whenever we have a polymorphic function

    f :: forall a. r[a] -> s[a] -> t[a]

we must instantiate `a` consistently: however the first argument fixes `a`, the second argument must play along, and we learn something useful about the result while we are at it. Allowing data at the type level is useful because some forms of consistency (e.g. lengths of things) are more readily expressed in terms of data (numbers).

But the real breakthrough is GADT pattern matching, where the type of a pattern can *refine* the type of the argument it matches. You have a vector of length `n`; you look to see whether it's nil or cons; now you know whether `n` is zero or not. This is a form of testing where the type of the code in each case is more specific than the type of the whole, because in each case something which has been *learned* is reflected at the type level. It is **learning by testing** which makes a language dependently typed, at least to some extent.

Here's a silly game to play, whatever typed language you use. Replace every type variable and every primitive type in your type expressions with 1 and evaluate types numerically (sum the sums, multiply the products, `s -> t` means `t`-to-the-`s`) and see what you get: if you get 0, you're a logician; if you get 1, you're a software engineer; if you get a power of 2, you're an electronic engineer; if you get infinity, you're a programmer. What's going on in this game is a crude attempt to measure the information we're managing and the choices our code must make. Our usual type systems are good at managing the "software engineering" aspects of coding: unpacking and plugging together components. But as soon as a choice has been made, there is no way for types to observe it, and as soon as there are choices to make, there is no way for types to guide us: non-dependent type systems approximate all values in a given type as the same. That's a pretty serious limitation on their use in bug prevention.

  [1]: https://personal.cis.strath.ac.uk/conor.mcbride/pub/hasochism.pdf
  [2]: https://github.com/slindley/dependent-haskell/tree/master/Hasochism
