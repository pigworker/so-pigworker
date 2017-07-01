In the differential calculus, Leibniz's notation causes less confusion than Newton's because it is explicit about the variable with respect to which we differentiate. Contexts in things are given by differentiation, so we must take care what is being contextualized. Here, there are two notions of "substructure" at work: *subtrees* and *elements*. They each have different (but related) notions of "context" and hence of "zipper", where a zipper is the pair of a thing and its context.

Your `BTZ` type is presented as the notion of zipper for *subtrees*. However, the zipper comonadic construction works on zippers for *elements*: `extract` means "give element here"; `duplicate` means "decorate each element with its context". So you need element contexts. Confusingly, for these binary trees, element zippers and subtree zippers are isomorphic, but that is for a very particular reason (namely that they form a cofree comonad).

Generally, element- and subtree-zippers differ, e.g., for lists. If we start by building the element-zipper comonad for lists, we are less likely to get lost when we come back to trees. Let me try also to fill in a bit more of the general picture, for others as well as yourself.

<h3>Sublist contexts</h3>

The *sublist*-contexts for `[a]` are just given by `[a]`, being the list of elements we pass by on the way out from the sublist to the whole list. The sublist context for `[3,4]` in `[1,2,3,4]` is `[2,1]`. Subnode contexts for recursive data are always lists representing what you see on the path from the node to the root. The type of each step is given by the partial derivative of the formula for one node of data with respect to the recursive variable. So here

    [a] = t where             -- t is the recursive variable standing for [a]
      t = 1 + a*t             -- lists of a are either [] or an (a : t) pair
    ∂/∂t (1 + a*t) = a        -- that's one step on a path from node to root
    sublist contexts are [a]  -- a list of such steps

So a sublist-zipper is a pair

    data LinLZ a = LinLZ
      {  subListCtxt  :: [a]
      ,  subList      :: [a]
      }

We can write the function which plugs a sublist back into its context, reversing back up the path

    plugLinLZ :: LinLZ a -> [a]
    plugLinLZ (LinLZ { subListCtxt = [],      subList = ys})  = ys
    plugLinLZ (LinLZ { subListCtxt = x : xs,  subList = ys})
      = plugLinLZ (LinLZ { subListCtxt = xs,  subList = x : ys})

But we can't make `LinLZ` a `Comonad`, because (for example) from

    LinLZ { subListCtxt = [], subList = [] }

we can't `extract` an *element* (an `a` from `LinLZ a`), only a sublist.

<h3>List Element Contexts</h3>

A list *element* context is a pair of lists: the elements before the element in focus, and the elements after it. An element context in a recursive structure is always a pair: first give the subnode-context for the subnode where the element is stored, then give the context for the element in its node. We get the element-in-its-node context by differentiating the formula for a node with respect to the variable which stands for elements.

    [a] = t where             -- t is the recursive variable standing for [a]
      t = 1 + a*t             -- lists of a are either [] or an (a : t) pair
    ∂/∂a (1 + a*t) = t = [a]  -- the context for the head element is the tail list

So an element context is given by a pair

    type DL a =
      (  [a]     -- the sublist context for the node where the element is
      ,  [a]     -- the tail of the node where the element is
      )

and an element zipper is given by pairing such a context with the element "in the hole".

    data ZL a = ZL
      {  this :: a
      ,  between :: DL a
      }  deriving (Show, Eq, Functor)

You can turn such a zipper back into a list (going "out" from an element) by first reconstituting the sublist where the element sits, giving us a sublist zipper, then plugging the sublist into its sublist-context.

    outZL :: ZL a -> [a]
    outZL (ZL { this = x, between = (zs, xs) })
      = plugLinLZ (LinLZ { subListCtxt = zs, subList = x : xs })


<h3>Putting each element into context</h3>

Given a list, we can pair each element up with its context. We get the list of ways we can "go into" one of the elements. We start like this,

    into :: [a] -> [ZL a]
    into xs = moreInto (LinLZ { subListCtxt = [], subList = xs })

but the real work is done by the helper function which works on a list-in-context.

    moreInto :: LinLZ a -> [ZL a]
    moreInto (LinLZ { subListCtxt = _,   subList = [] })      = []
    moreInto (LinLZ { subListCtxt = zs,  subList = x : xs })
      =  ZL { this = x, between = (zs, xs) } 
      :  moreInto (LinLZ { subListCtxt = x : zs,  subList = xs })

Notice that the output echoes the shape of the current `subList`. Also, the zipper in `x`'s place has
`this = x`. Also, the generating zipper for decorating `xs` has `subList = xs` and the correct context,
recording that we have moved past `x`. Testing,

    into [1,2,3,4] =
      [  ZL {this = 1, between = ([],[2,3,4])}
      ,  ZL {this = 2, between = ([1],[3,4])}
      ,  ZL {this = 3, between = ([2,1],[4])}
      ,  ZL {this = 4, between = ([3,2,1],[])}
      ]

<h3>Comonadic structure for list element zippers</h3>

We've seen how to go out from an element, or into one of the available elements. The comonadic structure tells us how to move between elements, either staying where we are, or moving to one of the others.

    instance Comonad ZL where

The `extract` gives us the element we're visiting.

      extract = this

To `duplicate` a zipper, we replace the current element `x` with the whole current zipper `zl` (whose `this = x`)...

      duplicate zl@(ZL { this = x, between = (zs, ys) }) = ZL
        {  this = zl

...and we work our way through the context, showing how to refocus at each element. Our existing `moreInto` lets us move inward, but we must also move `outward`...

        ,  between =
             (  outward (LinLZ { subListCtxt = zs, subList = x : ys })
             ,  moreInto (LinLZ { subListCtxt = x : zs, subList = ys })
             )
        }

...which involves travelling back along the context, moving elements into the sublist, as follows

        where
          outward (LinLZ { subListCtxt = [], subList = _ }) = []
          outward (LinLZ { subListCtxt = z : zs, subList = ys })
            =  ZL { this = z, between = (zs, ys) }
            :  outward (LinLZ { subListCtxt = zs, subList = z : ys })

So we get

    duplicate ZL {this = 2, between = ([1],[3,4])}
      = ZL
      {  this = ZL {this = 2, between = ([1],[3,4])}
      ,  between =
         (  [  ZL {this = 1, between = ([],[2,3,4])}  ]
         ,  [  ZL {this = 3, between = ([2,1],[4])}
            ,  ZL {this = 4, between = ([3,2,1],[])}
            ]
         )
      }

where `this` is "staying at `2`" and we are `between` "moving to `1`" and "moving to `3` or moving to `4`".

So, the comonadic structure shows us how we can move between different *elements* located inside a list. The sublist structure plays a key role in finding the nodes where the elements are, but the zipper structure `duplicate`d is an *element* zipper.

So what about trees?

<h3>Digression: labelled trees are comonads already</h3>

Let me refactor your type of binary trees to bring out some structure. Literally, let us pull the element which labels a leaf or a fork out as a common factor. Let us also isolate the functor (`TF`) which explains this leaf-or-fork subtree structure.

    data TF t = Leaf | Fork (t, t) deriving (Show, Eq, Functor)
    data BT a = a :& TF (BT a) deriving (Show, Eq, Functor)

That is, every tree node has a label, whether it is a leaf or a fork.

Wherever we have the structure that every node has a label and a blob of substructures, we have a comonad: the *cofree* *comonad*. Let me refactor a little more, abstracting out `TF`...

    data CoFree f a = a :& f (CoFree f a) deriving (Functor)

...so we have a general `f` where we had `TF` before. We can recover our specific trees.

    data TF t = Leaf | Fork (t, t) deriving (Show, Eq, Functor)
    type BT = CoFree TF
    deriving instance Show a => Show (BT a)
    deriving instance Eq a => Eq (BT a)

And now, once for all, we can give the cofree comonad construction. As every subtree has a root element, every element can be decorated with the tree whose root it is.

    instance Functor f => Comonad (CoFree f) where
      extract   (a :& _)     = a                         -- extract root element
      duplicate t@(a :& ft)  = t :& fmap duplicate ft    -- replace root element by whole tree

Let's have an example

    aTree =
      0 :& Fork
      (  1 :& Fork
         (  2 :& Leaf
         ,  3 :& Leaf
         )
      ,  4 :& Leaf
      )

    duplicate aTree =
      (0 :& Fork (1 :& Fork (2 :& Leaf,3 :& Leaf),4 :& Leaf)) :& Fork
      (  (1 :& Fork (2 :& Leaf,3 :& Leaf)) :& Fork
         (  (2 :& Leaf) :& Leaf
         ,  (3 :& Leaf) :& Leaf
         )
      ,  (4 :& Leaf) :& Leaf
      )

See? Each element has been paired with its subtree!

Lists do not give rise to a cofree comonad, because not every node has an element: specifically, `[]` has no element. In a cofree comonad, there is always an element where you are, and you can see further down into the tree structure, *but not further up*.

**In an element zipper comonad, there is always an element where you are, and you can see both up and down.**


<h3>Subtree and element contexts in binary trees</h3>

Algebraically

    d/dt (TF t) = d/dt (1 + t*t) = 0 + (1*t + t*1)

so we may define

    type DTF t = Either ((), t) (t, ())

saying that a subtree inside the "blob of substructures" is either on the left or the right. We can check that "plugging in" works.

    plugF :: t -> DTF t -> TF t
    plugF  t  (Left   ((), r))  = Fork (t, r)
    plugF  t  (Right  (l, ()))  = Fork (l, t)

If we instantiate `t` and pair up with the node label, we get one step of subtree context

    type BTStep a = (a, DTF (BT a))

which is isomorphic to `Partial` in the question.

    plugBTinBT :: BT a -> BTStep a -> BT a
    plugBTinBT t (a, d) = a :& plugF t d

So, a *subtree*-context for one `BT a` inside another is given by `[BTStep a]`.

But what about an *element* context? Well, every element labels some subtree, so we should record both that subtree's context and the rest of the tree labelled by the element.

    data DBT a = DBT
      {  below  :: TF (BT a)    -- the rest of the element's node
      ,  above  :: [BTStep a]   -- the subtree context of the element's node
      }  deriving (Show, Eq)

Annoyingly, I have to roll my own `Functor` instance.

    instance Functor DBT where
      fmap f (DBT { above = a, below = b }) = DBT
        {  below = fmap (fmap f) b
        ,  above = fmap (f *** (either
             (Left   . (id *** fmap f))
             (Right  . (fmap f *** id)))) a  
        }

Now I can say what an *element* zipper is.

    data BTZ a = BTZ
      {  here  :: a
      ,  ctxt  :: DBT a
      }  deriving (Show, Eq, Functor)

If you're thinking "what's new?", you're right. We have a subtree context, `above`, together with a subtree given by `here` and `below`. And that's because the only elements are those which label nodes.
Splitting a node up into an element and its context is the same as splitting it into its label and its blob of substructures. That is, we get this coincidence for cofree comonads, but not in general.

However, this coincidence is only a distraction! As we saw with lists, we don't need element-zippers to be the same as subnode-zippers to make element-zippers a comonad.

Following the same pattern as lists above, we can decorate every element with its context. The work is done by a helper function which accumulates the subtree context we are currently visiting.

    down :: BT a -> BT (BTZ a)
    down t = downIn t []

    downIn :: BT a -> [BTStep a] -> BT (BTZ a)
    downIn (a :& ft) ads =
      BTZ { here = a, ctxt = DBT { below = ft, above = ads } }
      :& furtherIn a ft ads

Note that `a` is replaced by a zipper focused on `a`. The subtrees are handled by another helper.

    furtherIn :: a -> TF (BT a) -> [BTStep a] -> TF (BT (BTZ a))
    furtherIn a Leaf           ads  = Leaf
    furtherIn a (Fork (l, r))  ads  = Fork
      (  downIn l ((a, Left   ((), r)) : ads)
      ,  downIn r ((a, Right  (l, ())) : ads)
      )

See that `furtherIn` preserves the tree structure, but grows the subtree context suitably when it visits a subtree.

Let's double check.

    down aTree =
      BTZ {  here  = 0, ctxt = DBT {
             below = Fork (1 :& Fork (2 :& Leaf,3 :& Leaf),4 :& Leaf),
             above = []}} :& Fork
      (  BTZ {  here = 1, ctxt = DBT {
                below = Fork (2 :& Leaf,3 :& Leaf),
                above = [(0,Left ((),4 :& Leaf))]}} :& Fork
         (  BTZ {  here = 2, ctxt = DBT {
                   below = Leaf,
                   above = [(1,Left ((),3 :& Leaf)),(0,Left ((),4 :& Leaf))]}} :& Leaf
         ,  BTZ {  here = 3, ctxt = DBT {
                   below = Leaf,
                   above = [(1,Right (2 :& Leaf,())),(0,Left ((),4 :& Leaf))]}} :& Leaf
         )
      ,  BTZ {  here = 4, ctxt = DBT {
                below = Leaf,
                above = [(0,Right (1 :& Fork (2 :& Leaf,3 :& Leaf),()))]}} :& Leaf)

See? Each element is decorated with its whole context, not just the tree below it.

<h3>Binary tree zippers form a Comonad</h3>

Now that we can decorate elements with their contexts, let us build the `Comonad` instance. As before...

    instance Comonad BTZ where
      extract = here

...`extract` tells us the element in focus, and we can make use of our existing machinery to go further into the tree, but we need to build new kit to explore the ways we can move outwards.

      duplicate z@(BTZ { here = a, ctxt = DBT { below = ft, above = ads }}) = BTZ
        {  here = z
        ,  ctxt = DBT
             {  below = furtherIn a ft ads  -- move somewhere below a
             ,  above = go_a (a :& ft) ads  -- go above a
             }
        } where

To go outwards, as with lists, we must move back along the path towards the root. As with lists, each step on the path is a place we can visit.

        go_a t []          = []
        go_a t (ad : ads)  = go_ad t ad ads : go_a (plugBTinBT t ad) ads
        go_ad t (a, d) ads =
          (  BTZ { here = a, ctxt = DBT { below = plugF t d, above = ads } }  -- visit here
          ,  go_d t a d ads                                                   -- try other subtree
          )

Unlike with lists, there are alternative branches along that path to explore. Wherever the path stores an unvisited subtree, we must decorate *its* elements with *their* contexts.

        go_d t a (Left ((), r)) ads = Left ((), downIn r ((a, Right (t, ())) : ads))
        go_d t a (Right (l, ())) ads = Right (downIn l ((a, Left ((), t)) : ads), ())

So now we've explained how to refocus from any element position to any other.

Let's see. Here we were visiting `1`:

    duplicate (BTZ {here = 1, ctxt = DBT {
                    below = Fork (2 :& Leaf,3 :& Leaf),
                    above = [(0,Left ((),4 :& Leaf))]}}) =
      BTZ {here = BTZ {here = 1, ctxt = DBT {
                       below = Fork (2 :& Leaf,3 :& Leaf),
                       above = [(0,Left ((),4 :& Leaf))]}}, ctxt = DBT {
           below = Fork (BTZ {here = 2, ctxt = DBT {
                              below = Leaf,
                              above = [(1,Left ((),3 :& Leaf)),(0,Left ((),4 :& Leaf))]}} :& Leaf
                        ,BTZ {here = 3, ctxt = DBT {
                              below = Leaf,
                              above = [(1,Right (2 :& Leaf,())),(0,Left ((),4 :& Leaf))]}} :& Leaf
                       ),
           above = [(BTZ {here = 0, ctxt = DBT {
                          below = Fork (1 :& Fork (2 :& Leaf,3 :& Leaf),4 :& Leaf),
                          above = []}}
                    ,Left ((),BTZ {here = 4, ctxt = DBT {
                                   below = Leaf,
                                   above = [(0,Right (1 :& Fork (2 :& Leaf,3 :& Leaf),()))]}} :& Leaf)
                    )
                   ]}}

By way of testing the comonad laws on a small sample of data, let us check:

    fmap (\ z -> extract (duplicate z) == z) (down aTree)
      = True :& Fork (True :& Fork (True :& Leaf,True :& Leaf),True :& Leaf)
    fmap (\ z -> fmap extract (duplicate z) == z) (down aTree)
      = True :& Fork (True :& Fork (True :& Leaf,True :& Leaf),True :& Leaf)
    fmap (\ z -> fmap duplicate (duplicate z) == duplicate (duplicate z)) (down aTree)
      = True :& Fork (True :& Fork (True :& Leaf,True :& Leaf),True :& Leaf)

