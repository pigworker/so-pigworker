You can assemble this function from pieces which either are standard or should be. The accepted answer has the right clue about zippers. My answer [about differentiation and comonads][1] gives a general treatment of the relevant operations, but let me be specific here.

I define the type of "lists with one element hole" as follows:

    data Bwd x = B0 | Bwd x :< x deriving Show
    type HoleyList x = (Bwd x, [x])

Strictly speaking, I don't need to introduce backward lists to do that, but I get so easily confused if I have to reverse things in my head. (It so happens that `HoleyList` is the formal derivative of `[]`.)

I can now define what it is to be a list element in its context.

    type InContext x = (HoleyList x, x)

The idea is that the second component of the pair belongs in between the backward list and the forward list. I can define the function which plugs the list back together (Called `upF` in the generic treatment.)

    plug :: InContext x -> [x]
    plug ((B0, xs), y)      = y : xs
    plug ((xz :< x, xs), y) = plug ((xz, y : xs), x)

I can also define the function that gives all the ways to take a list apart (`downF`, generically).

    selections :: [x] -> [InContext x]
    selections = go B0 where
      go xz [] = []
      go xz (x : xs) = ((xz, xs), x) : go (xz :< x) xs

Note that

    map snd  (selections xs) = xs 
    map plug (selections xs) = map (const xs) xs

And now we're good to follow Bartek's recipe.

    selectModify :: (a -> Bool) -> (a -> a) -> [a] -> [[a]]
    selectModify p f = map (plug . (id *** f)) . filter (p . snd) . selections

That is: filter the selections by the test, apply the function to the element in focus, then plug back together. If you have the zipper equipment lying about, it's a one-liner, and it should work for any differentiable functor, not just lists! Job done!

    > selectModify ((1 ==) . (`mod` 2)) (2*) [1..10]
    [[2,2,3,4,5,6,7,8,9,10]
    ,[1,2,6,4,5,6,7,8,9,10]
    ,[1,2,3,4,10,6,7,8,9,10]
    ,[1,2,3,4,5,6,14,8,9,10]
    ,[1,2,3,4,5,6,7,8,18,10]]

  [1]: https://stackoverflow.com/a/25572148/828361
