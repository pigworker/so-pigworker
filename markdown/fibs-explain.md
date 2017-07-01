Your "Because" is not telling the whole story. You're truncating the lists at "the story so far" and evaluating eagerly, then wondering where the rest comes from. That's not quite to grasp what's really going on, so good question.

What gets computed when you make the definition

    fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

? Very little. Computation starts once you begin to *use* the list. Lazy computation happens only on demand.

What's a demand? You get to ask "are you `[]` or `x : xs`?" and if it's the latter, you get a handle on the pieces.

When we ask that question of `fibs`, we get that

    fibs = x0 : xs0
    x0  = 0
    xs0 = 1 : zipWith (+) fibs (drop 1 fibs)

but that means (substituting for `fibs` and then `x0`)

    xs0 = 1 : zipWith (+) (0 : xs0) (drop 1 (0 : xs0))

and when we ask again, we get that

    xs0 = x1 : xs1
    x1  = 1
    xs1 = zipWith (+) (0 : xs0) (drop 1 (0 : xs0))

so

    xs1 = zipWith (+) (0 : 1 : xs1) (drop 1 (0 : 1 : xs1))

but now it gets interesting, because we have to do some work. Just enough work to answer the question, mind? When we look at `xs1`, we force `zipWith` which forces `drop`.

    xs1 = zipWith (+) (0 : 1 : xs1) (drop 1 (0 : 1 : xs1))
        = zipWith (+) (0 : 1 : xs1) (1 : xs1)
        = (0 + 1) : zipWith (+) (1 : xs1) xs1

so

    xs1 = x2 : xs2
    x2  = 0 + 1 = 1
    xs2 = zipWith (+) (1 : xs1) xs1
        = zipWith (+) (1 : 1 : xs2) (1 : xs2)

See? We've maintained that we still know the first two elements of one zipped list, and the first element of the other. That means we'll be able to deliver the next output *and* refresh our "buffer". When we look at `xs2`, we get

    xs2 = zipWith (+) (1 : 1 : xs2) (1 : xs2)
        = (1 + 1) : zipWith (1 : xs2) xs2
    xs2 = x3 : xs3
    x3  = 1 + 1 = 2
    xs3 = zipWith (1 : xs2) xs2
        = zipWith (1 : 2 : xs3) (2 : xs3)

and we're good to go again!

Each time we demand the next element, we also move one step further away from `zipWith` running out of elements, which is just as well, just in the nick of time.

None of the discipline that makes values show up in the nick of time is expressed in the types. At the moment, it's for programmers to make sure that well typed programs don't go wrong by running out of data when a demand is made. (I have plans to do something about that, but I'll try not to digress here.)

The key is that lazy, "on demand" computation means that we *don't* have to truncate lists to just the elements we can see when the process starts. We just need to know that we can always take the next step.
