Semiring typeclass and instances
================================

``` { .haskell}
{-# Language GeneralizedNewtypeDeriving #-}

module Semiring where

import Data.Monoid ( Monoid ( .. ) )
import Data.Ord    ( comparing )
```

The semiring typeclass. The same definition is used
[here](http://hackage.haskell.org/package/weighted-regexp).

``` { .haskell}
class Semiring s where
  (.+.), (.*.) :: s -> s -> s
  zero, one  :: s
```

We require that the following laws are satisfied for all `a, b, c :: s`:

-   `(.+.)` is associative, commutative and has `zero` as the neutral
    element.
-   `(.*.)` is associative and has `one` as the neutral element.
-   `(.*.)` distributes over `(.+.)` from both sides, i.e.

    `a .*. (b .+. c) == (a .*. b) .+. (a .*. c)`

    `(b .+. c) .*. a == (b .*. a) .+. (c .*. a)`
-   `zero` is annihilating, i.e.

    `a .*. zero == zero == zero .*. a`

Boolean values form a semiring.

``` { .haskell}
instance Semiring Bool where
  (.+.) = (||)
  (.*.) = (&&)
  zero  = False
  one   = True
```

"Numbers" form semirings as well.

``` {.haskell}
newtype Number n = Number { number :: n }
  deriving ( Num, Show, Eq )

instance Num n => Semiring (Number n) where
  (.+.) = (+)
  (.*.) = (*)
  zero  = 0
  one   = 1
```

Another classical semiring is the tropical semiring that can be used to
compute lightest paths. This semiring is usually defined on the
non-negative real numbers with infinity, but it can be easily
generalised.

``` { .haskell}
data Tropical w = MinWeight | MaxWeight | Weight { weight :: w }
    deriving Eq
```

``` { .haskell}
instance Ord w => Ord (Tropical w) where
    compare MinWeight  _           = LT
    compare _          MinWeight   = GT
    compare MaxWeight  _           = GT
    compare _          MaxWeight   = LT
    compare w          w'          = comparing weight w w'
```

`Tropical w` forms a semiring if `w` has an `Ord` and a `Monoid`
instance that satisfy `` forall x, y :: w . x <= x `mappend` y ``

This condition is *not* satisfied in case of the additive monoid of most
numerical values, because adding negative numbers makes values smaller.

``` { .haskell}
-- assuming the expansion property
instance (Ord w, Monoid w) => Semiring (Tropical w) where
    (.+.) = min
    zero  = MaxWeight
    one   = MinWeight

    MaxWeight .*. _         = MaxWeight
    _         .*. MaxWeight = MaxWeight
    MinWeight .*. x         = x
    x         .*. MinWeight = x
    Weight w  .*. Weight w' = Weight (w `mappend` w')
```
