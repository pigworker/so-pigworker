Here's another way of looking at it. Some of a function's arguments are invisible, others are visible. A type `input -> output` tells us that a visible `input` is expected as an argument. A type `(Constraint) => output` tells us that some invisible information is expected. They're not interchangeable, because the visible arguments must be written, and the invisible arguments must not be written. The invisible arguments are for the compiler to figure out for himself (well, he sounds like a himself to me), and he insists on puzzling them out: he refuses just to be told what they are!

Secretly, the full type of this `tell` example is

    tell :: forall (a :: *). (Show a) => [a] -> String

What I've done is to make clear where this `a` variable comes in and what kind of a thing it is. You can also read this as a "deal": `tell` offers to work for all types `a` which satisfy the demand `(Show a)`.

In order for a usage of `tell` to make sense, it needs three things. Two of them are invisible and one is visible. That is, when you use `tell`, you make the visibile argument explicit, and the compiler tries to fill in the invisible parts. Let's work through that type more slowly.

    tell :: forall (a :: *).   -- the type of elements to tell          (invisible)
            (Show a) =>        -- how to make a String from one element (invisible)
            [a] ->             -- the list of elements to be told       (visible)
            String             -- the String made by showing all the elements

So, when you use `tell`, e.g.,

    tell [True, False]

you give only the visible argument: the list `[True, False]` of things to tell, and the compiler figures out the invisible arguments. He knows that `True` and `False` are both values of type `Bool`, so that means

    [True, False] :: [Bool]

which is how the compiler figures out that the `a` in the type of `tell` must be `Bool`, making `[a] = [Bool]`

(By the way, about `[True, False] :: [Bool]`. Left of ::, the square brackets, [..], make list values. Right of ::, the square brackets, [..], make a type of lists. They may just look black on a grey background to you, but my brain colours the value-making brackets red and the type-making brackets blue. They are entirely different. I wish I could colour code on this site. I digress.)

So, now, the other invisible argument must satisfy this `(Show a)` thing, which we now know is specifically `(Show Bool)` because we figured out that `a` is `Bool`. We call this part of the type a "constraint", but really it's a demand not just that a fact is true, but that some useful stuff exists. The stuff that is being demanded here is that there is a function

    show :: Bool -> String

That's the function used to turn the individual elements `True` and `False` into `String`s in the process of evaluating `tell [True, False]`.

The identifier `Show` is the name of a *type class* and *show* is the method of that type class. A type `class` specifies an interface of operations which must be implemented for each `instance`. The invisible argument to the function is a record (or "dictionary") packaging the implementations of those operations for the type in question (here, the implementation of `show`). Without that information, the compiled code would have no way to do its job, but we don't have to write that information, because the compiler can (at least in this case) search through the instances it knows about and fill in the right one for the job.

So, we don't just have invisible type arguments (which are inferred at compile time, then erased before run time), signalled by lowercase type variables, or more explicitly by `forall` *blah* `.`. We also have invisible implementations of type class operations (which are looked up at compile time, to provide vital run time information). So, something very important happens *between inferring and erasing types*: while the compiler still knows the types, it uses them to figure out which invisible implementations will be needed at run time, so that we can get away with not writing them ourselves.

Zooming out, `=>` in a type documents our expectation that the compiler will make use of type information to guide the generation of run time code that we don't have to bother writing. That's a nice little bit of win, right there.

**Ulterior motive for type system hackers.** The message that the invisible-visible distinction is in a different place from the erasable-useful distinction is one that some people have not yet received. That is the classic Hindley-Milner position, but the fact is that these distinctions are orthogonal, and the sooner people learn to enjoy that, the better.
