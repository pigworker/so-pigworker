You *can* do this sort of thing with GADTs. Far be it from me to judge whether what results is a rabbit hole, but let me at least show the recipe. I'm using the new `PolyKinds` extension, but you can manage with less.

First, decide what sorts of stuff you will need, and define a datatype of those sorts.

    data Sort = Base | Compound

Next, define your data indexed by their sorts. It's like building a little typed language.

    data WeaponPart :: Sort -> * where
      WInt    :: Int ->                                   WeaponPart Base
      WHash   :: Map.Map String Int ->                    WeaponPart Base
      WNull   ::                                          WeaponPart Base
      WTrans  :: (Some WeaponPart -> Some WeaponPart) ->  WeaponPart Compound

You can represent &lsquo;data of any sort&rsquo; via existential quantification, as follows:

    data Some p where
      Wit :: p x -> Some p

Note that the `x` does not escape, but we can still inspect the &lsquo;evidence&rsquo; that `x` &lsquo;satisfies&rsquo; `p`. Note that `Some` must be a `data`type, not a `newtype` as GHC objects to existential `newtype`s.

You are now free to write `Sort`-generic operations. If you have generic inputs, you can just use polymorphism, effectively currying `Some p -> ...` as `forall x. p x -> ...`.

    instance Show (WeaponPart x) where
      show (WInt x)    = "WInt " ++ (show x)
      show (WHash x)   = "WHash " ++ (show x)
      show (WTrans _)  = "WTrans"
      show WNull       = "WNull"

The existential is needed for `Sort`-generic outputs: here I use it for input and output.

    cold :: Some WeaponPart -> Some WeaponPart
    cold (Wit (WInt x))    = Wit (WHash (Map.singleton "frost" x))
    cold (Wit (WHash x))   = Wit (WHash $ Map.insertWith (+) "frost" 5 x)
    cold (Wit (WTrans x))  = cold $ x (Wit (WInt 5))
    cold (Wit WNull)       = cold $ Wit (WInt 5)

I had to add the occasional touch of `Wit` about the place, but it's the same program.

Meanwhile, we can now write

    ofTheAbyss :: WeaponPart Compound -> Some WeaponPart
    ofTheAbyss (WTrans x) = x (Wit (WTrans x))

So it's not horrendous to work with embedded type systems. Sometimes there is a cost: if you want your embedded language to have *subsorting*, you may find you do extra computation just to change the index of some data's type, making no difference to the data themselves. If you don't need subsorting, the extra discipline can often be a real friend.
