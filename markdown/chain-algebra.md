Here's my thought process for this problem... We want to chop a list into &lsquo;chains&rsquo; (so a list of lists), given a test to see if two elements link up.

    chains :: (x -> x -> Bool) -> [x] -> [[x]]

I don't remember any such thing in the library, so I decide to roll my own. I want to identify a suitable recursion strategy for processing the list.

Can I just think about elements? No: I quickly rule out `map` and `foldMap`, as elements don't seem to be treated independently of each other in this problem.

Next, I ask &lsquo;Does the output type have a *list algebra*?&rsquo;. That may not sound like an obvious thing to think, phrased that way, but it unpacks to the following sensible question. Are there &lsquo;nil&rsquo; and &lsquo;cons&rsquo; operations that build up outputs (lists of chains), instead of inputs (lists)? If so, I can use `foldr` to transform input nil-and-cons into output nil-and-cons, like this.

    chains :: (x -> x -> Bool) -> [x] -> [[x]]
    chains link = foldr chCons chNil where
      -- chNil :: [[x]]
      -- chCons :: x -> [[x]] -> [[x]]

It's clear what `chNil` has to be, as I'm grouping the original elements. Empty in? Empty out!

    chains :: (x -> x -> Bool) -> [x] -> [[x]]
    chains link = foldr chCons [] where
      -- chCons :: x -> [[x]] -> [[x]]

Can I write `chCons`? Suppose I get a list of chains: how do I add a new element? Well, if there's a front chain I can link to then I should grow that chain, otherwise I should start a new chain. So I have a special case for a nonempty chain at the start of a nonempty list of chains, and a default to cons a singleton.

    chains :: (x -> x -> Bool) -> [x] -> [[x]]
    chains link = foldr chCons [] where
      chCons y (xs@(x : _) : xss) | link y x  = (y : xs) : xss
      chCons y xss                            = [y] : xss

And we're home!

    > chains (\ x y -> x + 1 == y) [1,2,3,4,5,6,8,9,10]
    [[1,2,3,4,5,6],[8,9,10]]

A bunch of operators has an *algebra* for a given type if you can implement those operators for values of that type. The constructors of a datatype are just one algebra, one implementation of a bunch of operators, building values in that very datatype. A good way to compute with inputs from a datatype is to implement its algebra for your desired type of outputs. The point of `foldr` is to capture this &lsquo;find the algebra&rsquo; pattern, and it's right on the money for this problem.
