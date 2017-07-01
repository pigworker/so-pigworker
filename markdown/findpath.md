Your language, about what you thought the program should *do*, suggests to me that you need help to escape from the trap of imperative thinking. Let me try to offer some help, based on thinking about what things *are*, not what things *do*.

For `findpath (Leaf y) x`, you're heading in the right direction. You just need to give `if` a lowercase `i`, and think about what the correct `Path` to a `Leaf` must be.

Now, let's think about the other possibility. You know more than that it's some `t`. You know that you're really trying to figure out what

    findpath (Node l r) x

is (what it `=`, indeed), because that's the other possibility for a `BTree`. Think of splitting the problem by asking "Is this `BTree` a `(Leaf y)` or a `(Node l r)`?" as one conceptual step of program design. Now, in order to figure out what the above left-hand side equals, you're entitled to some recursively computed information, namely what

    findpath l x

and

    findpath r x

are. If you know `Path` information for both `l` and `r`, can you say what the `Path` for the whole `Node l r` is? Let me rephrase that question by writing it in Haskell:

    findpath :: Eq a => BTree a -> a -> Path
    findpath (Leaf y)   x = if y==x then ??? else Nothing
    findpath (Node l r) x = nodepath (findpath l x) (findpath r x) where
      nodepath :: Path -> Path -> Path
      nodepath ???

I have expressed my question by introducing a *helper function* `nodepath` which takes as arguments the recursively computed information. Now you can try to implement `nodepath` by pattern matching on those two paths for the left and right subtrees, respectively. If you know whether they are `(Just p)` or `Nothing`, then you should be able to say what the path for the whole node must be.

Lesson one, the useful thoughts are of the form: "If this is like such-and-such, then that must be so-and-so.". Being, not doing.

Lesson two, the basic method of programming over a datatype is: split into constructor cases (`Leaf` versus `Node`, `Just` versus `Nothing`); collect useful information from any substructures by recursive calls; say what the value for the whole structure must be.

If you follow my advice and figure out what `nodepath` should be, you may find that it's simple enough not to merit being a separate named definition. In that case, just replace the `nodepath` call with its meaning and cut out the `where`-clause. But it's good to start by introducing `nodepath`, as it expresses a useful conceptual step towards solving the problem.
