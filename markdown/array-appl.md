Reading the comments, I'm a little worried that size is under the carpet here. Is there a sensible behaviour when sizes mismatch?

Meanwhile, there may be something you can sensibly do along the following lines. Even if your arrays aren't easy to make polymorphic, you can make an `Applicative` instance like this.

    data ArrayLike x = MkAL {sizeOf :: Int, eltOf :: Int -> x}

    instance Applicative ArrayLike where
      pure x                 = MkAL maxBound  (pure x)
      MkAL i f <*> MkAL j g  = MkAL (min i j) (f <*> g)

(Enthusiasts will note that I've taken the product of the `(Int ->)` applicative with that induced by the (`maxBound`, `min`) monoid.)

Could you make a clean correspondence

    imAL :: Image -> ArrayLike Float
    alIm :: ArrayLike Float -> Image

by projection and tabulation? If so, you can write code like this.

    alIm $ (f <$> imAL a1 <*> ... <*> imAL an)

Moreover, if you then want to wrap that pattern up as an overloaded operator,

    imapp :: (Float -> ... -> Float) -> (Image -> ... -> Image)

it's a standard exercise in typeclass programming! (Ask if you need more of a hint.)

The crucial point, though, is that the wrapping strategy means you don't need to monkey with your array structures in order to put functional superstructure on top.
