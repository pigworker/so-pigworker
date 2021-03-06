To see why you get the particular answers that arise, the desugaring explanations are very helpful. Let me supplement them with a little general advice about developing perceptions of Haskell code. 

Haskell's type system makes no distinction between two separable "moral" purposes:

  - `[x]` the type of *values* which are lists with elements drawn from `x`
  - `[x]` the type of *computations* of elements of `x` which allow prioritized choice

The fact that these two notions have the same representation does not mean that they play the same roles. In `f1`, the `[x, x+1]` is playing the role of computation, so the possibilities it generates are merged into the choice generated by the whole computation: that's what the `>>=` of the list monad does. In `f2`, however, the `[x, x+1]` is playing the role of value, so that the whole computation generates a prioritized choice between two values (which happen to be list values).

Haskell does not use types to make this distinction [and you may have guessed by now that I think it should, but that's another story]. Instead, it uses syntax. So you need to train your head to perceive the value and computation roles when you read code. The `do` notation is a special syntax for constructing *computations*. What goes inside the `do` is built from the following template kit:

![jigsaw pieces for computations][1]

The three blue pieces make `do`-computations. I've marked the computation holes in blue and the value holes in red. This is not meant to be a complete syntax, just a guide to how to perceive pieces of code in your mind.

Indeed, you may write any old expression in the blue places provided it has a suitably monadic type, and the computation so generated will be merged into the overall computation using `>>=` as needed. In your `f1` example, your list is in a blue place and treated as prioritized choice. 

Similarly, you may write expressions in red places which may very well have monadic types (like lists in this case), but they will be treated as values all the same. That's what happens in `f2`: as it were, the result's outer brackets are blue, but the inner brackets are red.

Train your brain to make the value/computation separation when you read code, so that you know instinctively which parts of the text are doing which job. Once you've reprogrammed your head, the distinction between `f1` and `f2` will seem completely normal!


  [1]: http://i.stack.imgur.com/BFJSD.jpg
