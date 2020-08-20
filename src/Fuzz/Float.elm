module Fuzz.Float exposing (fractionalFloat)

import Bitwise
import Elm.Kernel.Float


{-| Input: two 32bit integers
Output: Float (in the full range of IEEE-754 doubles)

This is doable in pure Elm, but quite slow.
JavaScript exposes a way to do the memory cast natively.

-}
fromBytes : ( Int, Int ) -> Float
fromBytes ( hi, lo ) =
    Elm.Kernel.Float.fromBytes hi lo


{-| Converts the two 32bit integers to a float 0..1 (never reaching 1).
Discards 12 most significant bits of `hi`.
-}
fractionalFloat : ( Int, Int ) -> Float
fractionalFloat ( hi, lo ) =
    {- Keep only the mantissa bits (0x000FFFFF 0xFFFFFFFF)
       and divide them by the maximal mantissa.
    -}
    fromBytes ( Bitwise.and 0x000FFFFF hi, lo )
        / maxMantissa


{-| Mantissa is the fractional part of the Float.
-}
maxMantissa : Float
maxMantissa =
    fromBytes ( 0x000FFFFF, 0xFFFFFFFF )
