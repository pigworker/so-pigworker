Let's just recap the definition of the `Cofree` datatype.

    data Cofree f a = a :< f (Cofree f a)

That's at least enough to diagnose the problem with the example. When you wrote

    1 :< [2, 3]

you made a small error that's reported rather more subtly than is helpful. Here, `f = []` and `a` is something numeric, because `1 :: a`. Correspondingly you need

    [2, 3] :: [Cofree [] a]

and hence

    2 :: Cofree [] a

which *could* be ok if `Cofree [] a` were also and instance of `Num`. Your definition thus acquires a constraint which is unlikely to be satisfied, and indeed, when you *use* your value, the attempt to satisfy the constraint fails.

Try again with

    1 :< [2 :< [], 3 :< []]

and you should have better luck.

Now, let's see what we've got. Start by keeping it simple. What's `Cofree f ()`? What, in particular, is `Cofree [] ()`? The latter is isomorphic to the fixpoint of `[]`: the tree structures where each node is a list of subtrees, also known as "unlabelled rose trees". E.g.,

    () :< [  () :< [  () :< []
                   ,  () :< []
                   ]
          ,  () :< []
          ]

Similarly, `Cofree Maybe ()` is more or less the fixpoint of `Maybe`: a copy of the natural numbers, because `Maybe` gives us either zero or one position into which to plug a subtree.

    zero :: Cofree Maybe ()
    zero = () :< Nothing
    succ :: Cofree Maybe () -> Cofree Maybe ()
    succ n = () :< Just n

An important trivial case is `Cofree (Const y) ()`, which is a copy of `y`. The `Const y` functor gives *no* positions for subtrees.

    pack :: y -> Cofree (Const y) ()
    pack y = () :< Const y

Next, let's get busy with the other parameter. It tells you what sort of label you attach to each node. Renaming the parameters more suggestively

    data Cofree nodeOf label = label :< nodeOf (Cofree nodeOf label)

When we label up the `(Const y)` example, we get *pairs*

    pair :: x -> y -> Cofree (Const y) x
    pair x y = x :< Const y

When we attach labels to the nodes of our numbers, we get *nonempty* lists

    one :: x -> Cofree Maybe x
    one = x :< Nothing
    cons :: x -> Cofree Maybe x -> Cofree Maybe x
    cons x xs = x :< Just xs

And for lists, we get *labelled* rose trees.

    0 :< [  1 :< [  3 :< []
                 ,  4 :< []
                 ]
         ,  2 :< []
         ]

These structures are always "nonempty", because there is at least a top node, even if it has no children, and that node will always have a label. The `extract` operation gives you the label of the top node.

    extract :: Cofree f a -> a
    extract (a :< _) = a

That is, `extract` throws away the *context* of the top label.

Now, the `duplicate` operation *decorates* every label with *its own* context.

    duplicate :: Cofree f a -> Cofree f (Cofree f a)
    duplicate a :< fca = (a :< fca) :< fmap duplicate fca  -- f's fmap

We can get a `Functor` instance for `Cofree f` by visiting the whole tree

    fmap :: (a -> b) -> Cofree f a -> Cofree f b
    fmap g (a :< fca) = g a :< fmap (fmap g) fca
        --                     ^^^^  ^^^^
        --                 f's fmap  ||||
        --                           (Cofree f)'s fmap, used recursively

It's not hard to see that

    fmap extract . duplicate = id

because `duplicate` decorates every node with its context, then `fmap extract` throws away the decoration.

Note that `fmap` gets to look only at the labels of the input to compute the labels of the output. Suppose we wanted to compute output labels depending on each input label *in its context*? E.g., given an unlabelled tree, we might want to label each node with the size of its entire subtree. Thanks to the `Foldable` instance for `Cofree f`, we should be able to count nodes with.

    length :: Foldable f => Cofree f a -> Int
 
So that means

    fmap length . duplicate :: Cofree f a -> Cofree f Int

The key idea of comonads is that they capture "things with some context", and they let you apply context-dependent maps everywhere.

    extend :: Comonad c => (c a -> b) -> c a -> c b
    extend f = fmap f       -- context-dependent map everywhere
               .            -- after
               duplicate    -- decorating everything with its context

Defining `extend` more directly saves you the trouble of duplication (although that amounts only to sharing).

    extend :: (Cofree f a -> b) -> Cofree f a -> Cofree f b
    extend g ca@(_ :< fca) = g ca :< fmap (extend g) fca

And you can get `duplicate` back by taking

    duplicate = extend id -- the output label is the input label in its context

Moreover, if you pick `extract` as the thing to do to each label-in-context, you just put each label back where it came from:

    extend extract = id

These "operations on labels-in-context" are called "co-Kleisli arrows",

    g :: c a -> b

and the job of `extend` is to interpret a co-Kleisli arrow as a function on whole structures. The `extract` operation is the identity co-Kleisli arrow, and it's interpreted by `extend` as the identity function. Of course, there is a co-Kleisli composition

    (=<=) :: Comonad c => (c s -> t) -> (c r -> s) -> (c r -> t)
    (g =<= h) = g . extend h

and the comonad laws ensure that `=<=` is associative and absorbs `extract`, giving us the co-Kleisli category. Moreover we have

    extend (g =<= h)  =  extend g . extend h

so that `extend` is a *functor* (in the categorical sense) from the co-Kleisli category to sets-and-functions. These laws are not hard to check for `Cofree`, as they follow from the `Functor` laws for the node shape.

Now, one useful way to see a structure in a cofree comonad is as a kind of "game server". A structure

    a :< fca

represents the state of the game. A move in the game consists either of "stopping", in which case you get the `a`, or of "continuing", by choosing a subtree of the `fca`. For example, consider

    Cofree ((->) move) prize

A client for this server must either stop, or continue by giving a `move`: it's a *list* of `move`s. The game is played out as follows:

    play :: [move] -> Cofree ((->) move) prize -> prize
    play []       (prize :< _) = prize
    play (m : ms) (_     :< f) = play ms (f m)

Perhaps a `move` is a `Char` and the `prize` is the result of parsing the character sequence.

If you stare hard enough, you'll see that `[move]` is a version of `Free ((,) move) ()`. Free monads represent client strategies. The functor `((,) move)` amounts to a command interface with only the command "send a `move`". The functor `((->) move)` is the corresponding structure "respond to the sending of a `move`".

Some functors can be seen as capturing a command interface; the free monad for such a functor represents programs that make commands; the functor will have a "dual" which represents how to respond to commands; the cofree comonad of the dual is the general notion of environment in which programs that make commands can be run, with the label saying what to do if the program stops and returns a value, and the substructures saying how to carry on running the program if it issues a command.

For example,

    data Comms x = Send Char x | Receive (Char -> x)

describes being allowed to send or receive characters. Its dual is

    data Responder x = Resp {ifSend :: Char -> x, ifReceive :: (Char, x)}

As an exercise, see if you can implement the interaction

    chatter :: Free Comms x -> Cofree Responder y -> (x, y)
