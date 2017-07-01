**tl;dr** `()` does not add a "null" value to every type, hell no; `()` is a "dull" value in a type of its own: `()`.

Let me step back from the question a moment and address a common source of confusion. A key thing to absorb when learning Haskell is the distinction between its *expression* language and its *type* language. You're probably aware that the two are kept separate. But that allows the same symbol to be used in both, and that is what is going on here. There are simple textual cues to tell you which language you're looking at. You don't need to parse the whole language to detect these cues.

The top level of a Haskell module lives, by default, in the expression language. You define functions by writing equations between expressions. But when you see *foo* :: *bar* in the expression language, it means that *foo* is an expression and *bar* is its type. So when you read `() :: ()`, you're seeing a statement which relates the `()` in the expression language with the `()` in the type language. The two `()` symbols mean different things, because they are not in the same language. This repetition often causes confusion for beginners, until the expression/type language separation installs itself in their subconscious, at which point it becomes helpfully mnemonic.

The keyword `data` introduces a new datatype declaration, involving a careful mixture of the expression and type languages, as it says first what the new type is, and secondly what its values are.

<pre>
data <i>TyCon tyvar ... tyvar</i> = <i>ValCon1 type ... type</i> |  ...  | <i>ValConn type ... type</i>
</pre>

In such a declaration, type constructor *TyCon* is being added to the type language and the *ValCon* value constructors are being added to the expression language (and its pattern sublanguage). In a `data` declaration, the things which stand in argument places for the *ValCon* s tell you the types given to the arguments when that *ValCon* is used in expressions. For example,

    data Tree a = Leaf | Node (Tree a) a (Tree a)

declares a type constructor `Tree` for binary tree types storing elements at nodes, whose values are given by value constructors `Leaf` and `Node`. I like to colour type constructors (Tree) blue and value constructors (Leaf, Node) red. There should be no blue in expressions and (unless you're using advanced features) no red in types. The built-in type `Bool` could be declared,

    data Bool = True | False

adding blue `Bool` to the type language, and red `True` and `False` to the expression language. Sadly, my markdown-fu is inadequate to the task of adding the colours to this post, so you'll just have to learn to add the colours in your head.

The "unit" type uses `()` as a special symbol, but it works as if declared

    data () = ()  -- the left () is blue; the right () is red

meaning that a notionally blue `()` is a type constructor in the type language, but that a notionally red `()` is a value constructor in the expression language, and indeed `() :: ()`. [It is not the only example of such a pun. The types of larger tuples follow the same pattern: pair syntax is as if given by

    data (a, b) = (a, b)

adding (,) to both type and expression languages. But I digress.

So the type `()`, often pronounced "Unit", is a type containing one value worth speaking of: that value is written `()` but in the expression language, and is sometimes pronounced "void". A type with only one value is not very interesting. A value of type `()` contributes zero bits of information: you already know what it must be. So, while there is nothing special about type `()` to indicate side effects, it often shows up as the value component in a monadic type. Monadic operations tend to have types which look like

<pre>
<i>val-in-type-1</i> -> ... -> <i>val-in-type-n</i> -> <i>effect-monad val-out-type</i>
</pre>

where the return type is a type application: the function tells you which effects are possible and the argument tells you what sort of value is produced by the operation. For example

    put :: s -> State s ()

which is read (because application associates to the left ["as we all did in the sixties", Roger Hindley]) as

    put :: s -> (State s) ()

has one value input type `s`, the effect-monad `State s`, and the value output type `()`. When you see `()` as a value output type, that just means "this operation is used only for its *effect*; the value delivered is uninteresting". Similarly

    putStr :: String -> IO ()

delivers a string to `stdout` but does not return anything exciting.

The `()` type is also useful as an element type for container-like structures, where it indicates that the data consists just of a *shape*, with no interesting payload. For example, if `Tree` is declared as above, then `Tree ()` is the type of binary tree shapes, storing nothing of interest at nodes. Similarly `[()]` is the type of lists of dull elements, and if there is nothing of interest in a list's elements, then the only information it contributes is its length.

To sum up, `()` is a type. Its one value, `()`, happens to have the same name, but that's ok because the type and expression languages are separate. It's useful to have a type representing "no information" because, in context (e.g., of a monad or a container), it tells you that only the context is interesting.
