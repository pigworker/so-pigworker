This is a variant of the notorious `show . read` problem. The classic version uses

    read :: Read a => String -> a
    show :: Show a => a -> String

so the composition might seem to be a plain old String transducer

    moo :: String -> String
    moo = show . read

except that there is no information in the program to determine the type in the middle, hence what to `read` and then `show`.

    Ambiguous type variable `b' in the constraints:
      `Read b' arising from a use of `read' at ...
      `Show b' arising from a use of `show' at ...
    Probable fix: add a type signature that fixes these type variable(s)

Please not that ghci does a bunch of crazy extra defaulting, resolving ambiguity arbitrarily.

    > (show . read) "()"
    "()"

Your `C` class is a variant of `Read`, except that it decodes an `Int` instead of reading a `String`, but the problem is essentially the same.

Type system enthusiasts will note that underconstrained type variables are not *per se* a big deal. It's ambiguous *instance inference* that's the issue here. Consider

    poo :: String -> a -> a
    poo _ = id

    qoo :: (a -> a) -> String
    qoo _ = ""

    roo :: String -> String
    roo = qoo . poo

In the construction of `roo`, it is never determined what the type in the middle must be, nor is `roo` polymorphic in that type. Type inference neither solves nor generalizes the variable! Even so,

    > roo "magoo"
    ""

it's not a problem, because the construction is *parametric* in the unknown type. The fact that the type cannot be determined has the consequence that the type cannot *matter*.

But unknown *instances* clearly do matter. The completeness result for Hindley-Milner type inference relies on parametricity and is thus lost when we add overloading. Let us not weep for it.
