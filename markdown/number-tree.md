First Attempt: Hard Work
------------------------

For the case of n-ary trees, there are *three* things going on: numbering elements, numbering trees, and numbering *lists* of trees. It would help to treat them separately. Types first:

    aNumber   :: a                -- thing to number
              -> Int              -- number to start from
              -> ( (a, Int)       -- numbered thing
                 , Int            -- next available number afterwards
                 )

    ntNumber  :: NT a             -- thing to number
              -> Int              -- number to start from
              -> ( NT (a, Int)    -- numbered thing
                 , Int            -- next available number afterwards
                 )

    ntsNumber :: [NT a]           -- thing to number
              -> Int              -- number to start from
              -> ( [NT (a, Int)]  -- numbered thing
                 , Int            -- next available number afterwards
                 )

Notice that all three types share the same pattern. When you see that there is a pattern that you are following, apparently by coincidence, you know you have an opportunity to learn something. But let's press on for now and learn later.

Numbering an element is easy: copy the starting number into the output and return its successor as the next available.

    aNumber a i = ((a, i), i + 1)

For the other two, the pattern (there's that word again) is

  1. split the input into its top-level components
  2. number each component in turn, threading the numbers through

It's easy to do the first with pattern matching (inspecting the data visually) and the second with `where` clauses (grabbing the two parts of the output).

For trees, a top level split gives us two components: an element and a list. In the where clause, we call the appropriate numbering functions as directed by those types. In each case, the "thing" output tells us what to put in place of the "thing" input. Meanwhile, we thread the numbers through, so the starting number for the whole is the starting number for the first component, the "next" number for the first component starts the second, and the "next" number from the second is the "next" number for the whole.

    ntNumber (N a ants) i0  = (N ai aints, i2) where
      (ai,    i1) = aNumber   a    i0
      (aints, i2) = ntsNumber ants i1

For lists, we have two possibilities. An empty list has no components, so we return it directly without using any more numbers. A "cons" has two components, we do exactly as we did before, using the appropriate numbering functions as directed by the type.

    ntsNumber []           i  = ([], i)
    ntsNumber (ant : ants) i0 = (aint : aints, i2) where
      (aint,  i1) = ntNumber  ant  i0
      (aints, i2) = ntsNumber ants i1

Let's give it a go.

    > let ntree = N "eric" [N "lea" [N "kristy" [],N "pedro" [] ,N "rafael" []],N "anna" [],N "bety" []]
    > ntNumber ntree 0
    (N ("eric",0) [N ("lea",1) [N ("kristy",2) [],N ("pedro",3) [],N ("rafael",4) []],N ("anna",5) [],N ("bety",6) []],7)

So we're there. But are we happy? Well, I'm not. I have the annoying sensation that I wrote pretty much the same type three times and pretty much the same program twice. And if I wanted to do more element-numbering for differently organised data (e.g., your binary trees), I'd have to write the same thing again again. Repetitive patterns in Haskell code are *always* missed opportunities: it's important to develop a sense of self-criticism and ask whether there's a neater way.

Second Attempt: Numbering and Threading
---------------------------------------

Two of the repetitive patterns we saw, above, are
  1. the similarity of the types,
  2. the similarity of the way the numbers get threaded.

If you match up the types to see what's in common, you'll notice they're all

    input -> Int -> (output, Int)

for different inputs and outputs. Let's give the largest common component a name.

    type Numbering output = Int -> (output, Int)

Now our three types are

    aNumber   :: a      -> Numbering (a, Int)
    ntNumber  :: NT a   -> Numbering (NT (a, Int))
    ntsNumber :: [NT a] -> Numbering [NT (a, Int)]

You often see such types in Haskell:

                 input  -> DoingStuffToGet output

Now, to deal with the threading, we can build some helpful tools to work with and combine `Numbering` operations. To see which tools we need, look at how we combine the outputs after we've numbered the components. The "thing" parts of the outputs are always built by applying some functions which don't get numbered (data constructors, usually) to some "thing" outputs from numberings.

To deal with the functions, we can build a gadget that looks a lot like our `[]` case, where no actual numbering was needed.

    steady :: thing -> Numbering thing
    steady x i = (x, i)

Don't be put off by the way the type makes it look as if `steady` has only one argument: remember that `Numbering thing` abbreviates a function type, so there really is another `->` in there. We get

    steady [] :: Numbering [a]
    steady [] i = ([], i)

just like in the first line of `ntsNumber`.

But what about the other constructors, `N` and `(:)`? Ask `ghci`.

    > :t steady N
    steady N :: Numbering (a -> [NT a] -> NT a)
    > :t steady (:)
    steady (:) :: Numbering (a -> [a] -> [a])

We get numbering operations with *functions* as outputs, and we want to generate the arguments to those function by more numbering operations, producing one big overall numbering operation with the numbers threaded through. One step of that process is to feed a numbering-generated function one numbering-generated input. I'll define that as an infix operator.

    ($$) :: Numbering (a -> b) -> Numbering a -> Numbering b
    infixl 2 $$

Compare with the type of the explicit application operator, `$`

    > :t ($)
    ($) :: (a -> b) -> a -> b

This `$$` operator is "application for numberings". If we can get it right, our code becomes

    ntNumber  :: NT a -> Numbering (NT (a, Int))
    ntNumber  (N a ants)   i = (steady N $$ aNumber a $$ ntsNumber ants) i

    ntsNumber :: [NT a] -> Numbering [NT (a, Int)]
    ntsNumber []           i = steady [] i
    ntsNumber (ant : ants) i = (steady (:) $$ ntNumber ant $$ ntsNumber ants) i

with `aNumber` as it was (for the moment). This code just does the data reconstruction, plugging together the constructors and the numbering processes for the components. We had better give the definition of `$$` and make sure it gets the threading right.

    ($$) :: Numbering (a -> b) -> Numbering a -> Numbering b
    (fn $$ an) i0 = (f a, i2) where
      (f, i1) = fn i0
      (a, i2) = an i1

Here, our old threading *pattern* gets done *once*. Each of `fn` and `an` is a function, expecting a starting number, and the whole of `fn $$ sn` is a function, which gets the starting number `i0`. We thread the numbers through, collecting first the function, then the argument. We then do the actual application and hand back the final "next" number.

Now, notice that in every line of code, the `i` input is fed in as the argument to a numbering process. We can simplify this code by just talking about the *processes*, not the *numbers*.

    ntNumber  :: NT a -> Numbering (NT (a, Int))
    ntNumber  (N a ants)   = steady N $$ aNumber a $$ ntsNumber ants

    ntsNumber :: [NT a] -> Numbering [NT (a, Int)]
    ntsNumber []           = steady []
    ntsNumber (ant : ants) = steady (:) $$ ntNumber ant $$ ntsNumber ants

One way to read this code is to filter out all the `Numbering`, `steady` and `$$` uses.

    ntNumber  :: NT a -> ......... (NT (a, Int))
    ntNumber  (N a ants)   = ...... N .. (aNumber a) .. (ntsNumber ants)

    ntsNumber :: [NT a] -> ......... [NT (a, Int)]
    ntsNumber []           = ...... []
    ntsNumber (ant : ants) = ...... (:) .. (ntNumber ant) .. (ntsNumber ants)

and you see it just looks like a preorder traversal, reconstructing the original data structure after processing the elements. We're doing the right thing with the *values*, provided `steady` and `$$` are correctly combining the *processes*.

We could try to do the same for `aNumber`

    aNumber  :: a -> Numbering a
    aNumber a = steady (,) $$ steady a $$ ????

but the `????` is where we actually need the number. We could build a numbering process that fits in that hole: a numbering process that *issues the next number*.

    next :: Numbering Int
    next i = (i, i + 1)

That's the essence of numbering, the "thing" output is the number to be used now (which is the starting number), and the "next" number output is the one after. We may write

    aNumber a = steady (,) $$ steady a $$ next

which simplifies to

    aNumber a = steady ((,) a) $$ next

In our filtered view, that's

    aNumber a = ...... ((,) a) .. next

What we've done is to bottle the idea of a "numbering process" and we've built the right tools to do *ordinary functional programming* with those processes. The threading pattern turns into the definitions of `steady` and `$$`.

Numbering is not the only thing that works this way. Try this...

    > :info Applicative
    class Functor f => Applicative (f :: * -> *) where
      pure :: a -> f a
      (<*>) :: f (a -> b) -> f a -> f b

...and you also get some more stuff. I just want to draw attention to the types of `pure` and `<*>`. They're a lot like `steady` and `$$`, but they are not just for `Numbering`. `Applicative` is the type class for *every* kind of process which works that way. I'm not saying "learn `Applicative` now!", just suggesting a direction of travel.

Third Attempt: Type-Directed Numbering
--------------------------------------

So far, our solution is directed towards one particular data structure, `NT a`, with `[NT a]` showing up as an auxiliary notion because it's used in `NT a`. We can make the whole thing a bit more plug-and-play if we focus on one layer of the type at a time. We defined numbering a list of trees in terms of numbering trees. In general, we know how to number a list of *stuff* if we know how to number each item of *stuff*.

If we know how to number an `a` to get `b`, we should be able to number a *list* of `a` to get a *list* of `b`. We can abstract over "how to process each item".

    listNumber :: (a -> Numbering b) -> [a] -> Numbering [b]
    listNumber na []       = steady []
    listNumber na (a : as) = steady (:) $$ na a $$ listNumber na as

and now our old list-of-trees-numbering function becomes

    ntsNumber :: [NT a] -> Numbering [NT (a, Int)]
    ntsNumber = listNumber ntNumber

which is hardly worth naming. We can just write

    ntNumber :: NT a -> Numbering (NT (a, Int))
    ntNumber (N a ants) = steady N $$ aNumber a $$ listNumber ntNumber ants

We can play the same game for the trees themselves. If you know how to number stuff, you know how to number a tree of stuff.

    ntNumber' :: (a -> Numbering b) -> NT a -> Numbering (NT b)
    ntNumber' na (N a ants) = steady N $$ na a $$ listNumber (ntNumber' na) ants

Now we can do things like this

    myTree :: NT [String]
    myTree = N ["a", "b", "c"] [N ["d", "e"] [], N ["f"] []]

    > ntNumber' (listNumber aNumber) myTree 0
    (N [("a",0),("b",1),("c",2)] [N [("d",3),("e",4)] [],N [("f",5)] []],6)

Here, the node data is now itself a list of things, but we've been able to number those things individually. Our equipment is more adaptable because each component aligns with one layer of the type.

Now, try this:

    > :t traverse
    traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)

It's an awful lot like the thing we just did, where `f` is `Numbering` and `t` is sometimes lists and sometimes trees.

The `Traversable` class captures what it means to be a type-former that lets you thread some sort of process through the stored elements. Again, the pattern you're using is very common and has been anticipated. Learning to use `traverse` saves a lot of work.

Eventually...
-------------

...you'll learn that a thing to do the job of `Numbering` already exists in the library: it's called `State Int` and it belongs to the `Monad` class, which means it must also be in the `Applicative` class. To get hold of it,

    import Control.Monad.State

and the operation which kicks off a stateful process with its initial state, like our feeding-in of `0`, is this thing:

    > :t evalState
    evalState :: State s a -> s -> a

Our `next` operation becomes

    next' :: State Int Int
    next' = get <* modify (1+)

where `get` is the process that accesses the state, `modify` makes a process that changes the state, and `<*` means "but also do".

If you start you file with the language extension pragma

    {-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

you can declare your datatype like this

    data NT a = N a [NT a] deriving (Show, Functor, Foldable, Traversable)

and Haskell will write `traverse` for you.

Your program then becomes one line...

    evalState (traverse (\ a -> pure ((,) a) <*> get <* modify (1+)) ntree) 0
    --                  ^ how to process one element ^^^^^^^^^^^^^^^
    --         ^ how to process an entire tree of elements ^^^^^^^^^
    --        ^ processing your particular tree ^^^^^^^^^^^^^^^^^^^^^^^^^^^
    -- ^ kicking off the process with a starting number of 0 ^^^^^^^^^^^^^^^^

...but the journey to that one line involves a lot of "bottling the pattern" steps, which takes some (hopefully rewarding) learning.
