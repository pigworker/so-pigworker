I don't know whether this suggestion is "good" for sophisticated definitions of "good", but it's easy and fun. I often set an exercise to write the core of a text editor in Haskell, linking with rendering code that I provide. The data model is as follows.

First, I define what it is to be a cursor inside a list of `x`-elements, where the information available at the cursor has some type `m`. (The `x` will turn out to be `Char` or `String`.)

<!-- language: haskell -->

    type Cursor x m = (Bwd x, m, [x])

This `Bwd` thing is just the backward "snoc-lists". I want to keep strong spatial intuitions, so I turn things around in my code, not in my head. The idea is that the stuff nearest the cursor is the most easily accessible. That's the spirit of The Zipper.

<!-- language: haskell -->

    data Bwd x = B0 | Bwd x :< x deriving (Show, Eq)

I provide a gratuitous singleton type to act as a readable marker for the cursor...

<!-- language: haskell -->

    data Here = Here deriving Show

...and I can thus say what it is to be somewhere in a `String`

<!-- language: haskell -->

    type StringCursor = Cursor Char Here

Now, to represent a buffer of multiple lines, we need `String`s above and below the line with the cursor, and a `StringCursor` in the middle, for the line we're currently editing.

<!-- language: haskell -->

    type TextCursor = Cursor String StringCursor

This `TextCursor` type is all I use to represent the state of the edit buffer. It's a two layer zipper. I provide the students with code to render a viewport on the text in an ANSI-escape-enabled shell window, ensuring that the viewport contains the cursor. All they have to do is implement the code that updates the `TextCursor` in response to keystrokes.

<!-- language: haskell -->

    handleKey :: Key -> TextCursor -> Maybe (Damage, TextCursor)

where `handleKey` should return `Nothing` if the keystroke is meaningless, but otherwise deliver `Just` an updated `TextCursor` and a "damage report", the latter being one of

<!-- language: haskell -->

    data Damage
      = NoChange       -- use this if nothing at all happened
      | PointChanged   -- use this if you moved the cursor but kept the text
      | LineChanged    -- use this if you changed text only on the current line
      | LotsChanged    -- use this if you changed text off the current line
      deriving (Show, Eq, Ord)

(If you're wondering what the difference is between returning `Nothing` and returning `Just (NoChange, ...)`, consider whether you also want the editor to go beep.) The damage report tells the renderer how much work it needs to do to bring the displayed image up to date.

The `Key` type just gives a readable dataype representation to the possible keystrokes, abstracting away from the raw ANSI escape sequences. It's unremarkable.

I provide the students with a big clue about to go up and down with this data model by offering these pieces of kit:

<!-- language: haskell -->

    deactivate :: Cursor x Here -> (Int, [x])
    deactivate c = outward 0 c where
      outward i (B0, Here, xs)       = (i, xs)
      outward i (xz :< x, Here, xs)  = outward (i + 1) (xz, Here, x : xs)

The `deactivate` function is used to shift focus out of a `Cursor`, giving you an ordinary list, but telling you where the cursor *was*. The corresponding `activate` function attempts to place the cursor at a given position in a list:

<!-- language: haskell -->

    activate :: (Int, [x]) -> Cursor x Here
    activate (i, xs) = inward i (B0, Here, xs) where
      inward _ c@(_, Here, [])     = c  -- we can go no further
      inward 0 c                   = c  -- we should go no further
      inward i (xz, Here, x : xs)  = inward (i - 1) (xz :< x, Here, xs)  -- and on!

I offer the students a deliberately incorrect and incomplete definition of `handleKey`

<!-- language: haskell -->

    handleKey :: Key -> TextCursor -> Maybe (Damage, TextCursor)
    handleKey (CharKey c)  (sz,
                            (cz, Here, cs),
                            ss)
      = Just (LineChanged, (sz,
                            (cz, Here, c : cs),
                            ss))
    handleKey _ _ = Nothing

which just handles ordinary character keystrokes but makes the text come out backwards. It's easy to see that the character `c` appears *right* of `Here`. I invite them to fix the bug and add functionality for the arrow keys, backspace, delete, return, and so on.

It may not be the most efficient representation ever, but it's purely functional and enables the code to conform concretely to our spatial intuitions about the text that's being edited.
