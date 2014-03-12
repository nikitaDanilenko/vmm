%Random Matrices as Pure Association Lists

This module provides a simple generation of different types of random matrices (e.g. triangle or
diagonal matrices). The randomness is provided by the `System.Random`{.haskell} implementation and
is thus repeatable, which makes it very useful for testing.

\begin{code}
module RandomMatrix (
    randomMatLike,
    randomSquareMatLike,
    randomDiagonalLike,
    randomTriangleLike,
    randomStrictTriangleLike,
    randomRelationLike,

    MatLike,

    -- * Re-export from "System.Random" to simplify testing
    mkStdGen,
    StdGen,
    Random
    ) where

import Data.Maybe            ( mapMaybe )

import System.Random         ( Random (..), RandomGen, StdGen, split, mkStdGen )
import System.Random.Shuffle ( shuffle' )

import Semiring              ( Number ( .. ) )
\end{code}

Generation of random matrices
-----------------------------

For simplicity only the "pure association lists" of matrices are generated, which are represented
by the stacked association lists. Values of the type `MatLike a`{.haskell} are required to satisfy
the following two conditions:

* Its indices are exactly $0$ to $n - 1$ for some $n \in \mathbb N$ in precisely this order.
* The value at every index is an association list that is sorted with respect to its indices.

\begin{code}
type MatLike a = [(Int, [(Int, a)])]
\end{code}

This function generates the necessary values. First, it creates a table of the right size and fills
it with the correct number of "non-zero" values at the first positions and with "zeroes" at the
remaining positions. Then it uses the supplied random generator to shuffle the table. Finally, the
table is reduced to an association list.
Any density larger than 1 behaves as 1 and every density smaller than 0 behaves as 0.

\begin{code}
randomMatLikeWith :: (RandomGen g, Random a) =>
    g                        -- ^ random generator
 -> Int                      -- ^ number of rows
 -> Int                      -- ^ number of columns
 -> (Int -> Int -> Int)      -- ^ function that computes the number of entries
                             --   in the matrix, i.e. @(*)@
 -> ([Maybe a] -> MatLike a) -- ^ function that splits the overall list into
                             --   sublists which then become rows
 -> Double                   -- ^ density, i.e. percentage of edges, which is
                             --   a 'Double' value between /0/ and /1/
 -> (a, a)                   -- ^ Lower\/upper bounds for the random values
 -> MatLike a
randomMatLikeWith g rs cs size resize d lu = resize shuffled where
    shuffled = shuffle' toGo entries g2
    entries  = size cs rs
    fill     = floor (fromIntegral entries * d)
    toGo     =    map Just (take fill (randomRs lu g1)) -- \"interesting\" values
               ++ replicate (entries - fill) Nothing    -- \"zeroes\"
    (g1, g2) = split g
\end{code}

This function creates a random matrix by computing the necessary number of entries,
then shuffling them and finally splitting them into uniform chunks which are then
used as rows.

\begin{code}
randomMatLike ::
    (RandomGen g, Random a) => g            -- ^ random generator
                            -> Int          -- ^ number of rows
                            -> Int          -- ^ number of colums
                            -> Double       -- ^ density (/0 <= d <= 1/)
                            -> (a, a)       -- ^ lower\/upper bounds
                            -> MatLike a
randomMatLike gen rows cols = randomMatLikeWith gen rows cols (*) (resizeWith (chopUniform cols))
\end{code}

A random graph is a random matrix with the same number of rows and columns.

\begin{code}
randomSquareMatLike ::
    (RandomGen g, Random a) => g            -- ^ random generator
                            -> Int          -- ^ number of rows and columns
                            -> Double       -- ^ density (/0 <= d <= 1/)
                            -> (a, a)       -- ^ lower\/upper bounds
                            -> MatLike a
randomSquareMatLike gen size = randomMatLike gen size size
\end{code}

A random relation is a special case of a matrix where entries are either existent or not.
Existent entries are denoted by the value (), non-existent entries are simply not contained
in the corresponding list.

\begin{code}
randomRelationLike ::
    RandomGen g => g              -- ^ random generator
                -> Int            -- ^ number of rows
                -> Int            -- ^ number of columns
                -> Double         -- ^ density (/0 <= d <= 1/)
                -> MatLike ()
randomRelationLike gen rows cols dens = randomMatLike gen rows cols dens ((), ())
\end{code}

Creates a random diagonal square matrix. Please note that the density is
computed w.r.t. the diagonal and /not/ the number of entries altogether. That
is: `randomDiagonal (mkStdGen 1234) 10 0.3 (0, 1)`{.haskell} will create a square matrix
with exactly three (not thirty) entries.

\begin{code}
randomDiagonalLike ::
    (RandomGen g, Random a) => g            -- ^ random generator
                            -> Int          -- ^ number of rows and columns
                            -> Double       -- ^ density (/0 <= d <= 1/)
                            -> (a, a)       -- ^ lower\/upper bounds
                            -> MatLike a
randomDiagonalLike gen size = randomMatLikeWith gen size size const resize where

    resize = zipWith (\i mv -> (i, maybe [] (\v -> [(i, v)]) mv)) [0..]
\end{code}

Creates a random triangle square matrix. As with `randomDiagonal`{.haskell} the density
refers to the density of the triangle. That is the number of entries in the matrix will
be `floor (density * size * (size + 1) / 2)`{.haskell}.

\begin{code}
randomTriangleLike ::
    (RandomGen g, Random a) => g            -- ^ random generator
                            -> Int          -- ^ number of rows and columns
                            -> Double       -- ^ density (/0 <= d <= 1/)
                            -> (a, a)       -- ^ lower\/upper bounds
                            -> MatLike a
randomTriangleLike gen size = randomMatLikeWith gen size size f (resizeWith chopTriangle) where

    f n _ = n * (n + 1) `div` 2
\end{code}

Creates a random strict triangle matrix (no entries at the diagonal). The density refers to the
density of the strict triangle, that is the number of entries is
'floor (density * size * (size - 1) / 2)`{.haskell}.

\begin{code}
randomStrictTriangleLike ::
    (RandomGen g, Random a) => g            -- ^ random generator
                            -> Int          -- ^ number of rows and columns
                            -> Double       -- ^ density (/0 <= d <= 1/)
                            -> (a, a)       -- ^ lower\/upper bounds
                            -> MatLike a
randomStrictTriangleLike gen size = randomMatLikeWith gen size size f (resizeWith chopStrictTriangle)
    where f n _ = n * (n - 1) `div` 2
\end{code}

Random instances
----------------

`Random`{.haskell} instance for pairs of Random instances that simply generates two values in
sequence.

\begin{code}
instance (Random a, Random b) => Random (a, b) where

    randomR ((la, lb), (ua, ub)) g = ((x, y), g'')
        where (x, g')  = randomR (la, ua) g
              (y, g'') = randomR (lb, ub) g'

    random g = ((x, y), g'') where
        (x, g')  = random g
        (y, g'') = random g'
\end{code}

Random instance for the semiring of `Number`{.haskell}s, which simply wraps the creation of
a value into a `Number`{.haskell} constructor.

\begin{code}
instance Random a => Random (Number a) where

    randomR (l, u) g = (Number x, g')
        where (x, g') = randomR (number l, number u) g

    random g = (Number x, g')
        where (x, g') = random g
\end{code}

Random instance for `()`{.haskell}, which is trivial and deterministic.

\begin{code}
instance Random () where

    random g   = ((), fst (split g))
    randomR _  = random
    randoms _  = repeat ()
    randomRs _ = randoms
\end{code}

Auxiliary functions
-------------------

Given a function, an integer and a list, this function breaks the list in sublists. The given
function is used to determine the length of the next chunk. For instance:

* `breakWith id    3 "Explanation" == ["Exp","lan","ati","on"]`{.haskell}
* `breakWith (+ 1) 1 "Explanation" == ["E","xp","lan","atio","n"]`{.haskell}

\begin{code}
breakWith :: (Int -> Int) -> Int -> [a] -> [[a]]
breakWith f = go where

    go _ [] = []
    go n xs = take n xs : go (f n) (drop n xs)
\end{code}

Given an integer $n$ and a list this function breaks the list into chunks of length $n$. The
last chunk is shorter, iff the length of the given list is not a multiple of $n$.

\begin{code}
chopUniform :: Int -> [a] -> [[a]]
chopUniform = breakWith id
\end{code}

This function chops a given list into sublists of increasing length beginning with 1. The last
element is shorter than the second to last, if the length of the list is not $n\cdot(n+1)/2$ for
some $n \in \mathbb N$.

The name of the function hints at its use, since one can use the resulting chunks to fill a
lower triangle matrix.

\begin{code}
chopTriangle :: [a] -> [[a]]
chopTriangle = breakWith (+ 1) 1
\end{code}

This function behaves very similarly to `chopTriangle`{.haskell}, but its first list is empty.
The last element of this list is shorter than the second to last iff the list length is not
$n\cdot(n+1)/2$ for some integer $n \in \mathbb N$.

Again, the name hints at the function's application, namely the construction of a strict lower
triangle matrix.

\begin{code}
chopStrictTriangle :: [a] -> [[a]]
chopStrictTriangle = breakWith (+1) 0
\end{code}

One particular recurring scheme is to split a list of `Maybe a` values into lists of such values
and then transform these lists into rows. This scheme is captured by the following function.

\begin{code}
resizeWith :: ([Maybe a] -> [[Maybe a]]) -> [Maybe a] -> MatLike a
resizeWith f = zip [0 .. ] . map toRow . f
\end{code}

This function transforms a list of `Maybe`{.haskell} values into an association list by first
indexing the list and then removing the `Nothing`{.haskell} values. For example,

* `toRow [Just 'h', Nothing, Just 'i'] == [(0, 'h'), (2, 'i')]`{.haskell}

\begin{code}
toRow :: [Maybe a] -> [(Int, a)]
toRow = mapMaybe (uncurry (fmap . (,))) . zip [0 .. ]
\end{code}