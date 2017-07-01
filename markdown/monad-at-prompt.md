As things stand, the `IO`-specific behaviour relies on the way `IO` actions are a bit statelike and unretractable. So you can say things like

    s <- readFile "foo.txt"

and get an actual value `s :: String`.

It's pretty clear that it takes more than just `Monad` structure to sustain that sort of interaction. It would not be so easy with

    n <- [1, 2, 3]

to say what *value* n has.

One could certainly imagine adapting ghci to open a prompt allowing a monadic computation to be constructed `do`-style in multiple command-line interactions, delivering the whole computation when the prompt is closed. It's not clear what it would mean to inspect the intermediate values (other than to generate collections of printing computations of type `m (IO ())`, for the active monad `m`, of course).

But it would be interesting to ask whether what's special about `IO` that makes a nice interactive prompt behaviour possible can be isolated and generalized. I can't help sniffing a whiff of a comonadic value-in-context story about interaction at a prompt, but I haven't tracked it down yet. One might imagine addressing my list example by considering what it would mean to have a *cursor* into a space of possible values, the way `IO` has a cursor imposed on it by the here-and-now of the real world. Thank you for the food for thought.
