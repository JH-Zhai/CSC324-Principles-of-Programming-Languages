module RIVS where

-- A “real inner-product vector space” is a vector space that has an inner
-- product (e.g., dot product for ℝ²), using real numbers for scalars.  It
-- supports the following operations; to help state their names and types, I
-- write “V” for the vector space in question, and for convenience I use Double
-- for real numbers:
--
-- * a zero vector
--       zero :: V
--
-- * adding two vectors
--       plus :: V -> V -> V
--
-- * the negative of a vector
--       neg :: V -> V
--   So that, e.g., plus v (neg v) = zero
--
-- * subtracting two vectors
--       minus :: V -> V -> V
--
-- * multiplying a scalar (real number in our case) to a vector. Since this
--   scales up or down a vector, I call it “scale”.  Perhaps this explains the
--   name “scalar” too.
--       scale :: Double -> V -> V
--
-- * inner product of two vectors, resulting in a scalar (real number for us).
--   I call it “dot” because it's shorter:
--       dot :: V -> V -> Double
--
-- There are a ton of axioms too, but we won't worry about them this time.


-- Part (a)
-----------
--
-- Define the type class RIVS below for the operations above, i.e., the
-- operations should be the methods of RIVS.  Keep using Double for the scalar
-- type.
--
-- Include default implementations for neg and minus. Recall that they are
-- closely related by:
--
--     - v = 0 - v
--     v - w = v + (- w)
--
-- If you don't like the type variable name “a”, you are free to change it, just
-- make sure it starts with a lowercase letter.

class RIVS a where
    zero :: a
    plus :: a -> a -> a
    neg :: a -> a
    minus :: a -> a -> a
    scale :: Double -> a -> a
    dot :: a -> a -> Double

    minus u v = plus u (neg v)
    neg v = minus zero v


-- The benefit of having RIVS is that algorithms for inner-product spaces can be
-- coded up polymorphically:

-- Assume that ws is an orthonormal list of vectors.
-- project v ws = v projected on to the “plane” spanned by ws.
project :: RIVS a => a -> [a] -> a
project v ws = foldl plus zero [scale (dot v w) w | w <- ws]

-- The Gram-Schmidt algorithm converts a linearly independent list of vectors to
-- an orthonormal list of vectors of the same span. In other words, compute an
-- orthonormal basis.
gramschmidt :: RIVS a => [a] -> [a]
gramschmidt [] = []
gramschmidt (v:vs) =
  let bs = gramschmidt vs
      w = minus v (project v bs)
      wlen :: Double
      wlen = sqrt (dot w w)
  in scale (1/wlen) w : bs


-- Part (b)
-----------
--
-- ℝ is a simple real inner-product vector spaces. Make Double an instance of
-- RIVS accordingly.
--
-- Since minus and neg have default implementations using each other, you only
-- need to provide one of them in an instance. Of course it is also harmless to
-- provide both.

instance RIVS Double where
    zero = 0
    plus = (+)
    scale = (*)
    dot = (*)
    -- need just one of the following two
    minus = (-)
    neg = negate


-- Part (c)
-----------
--
-- ℝ² is a well known real inner-product vector space, and is represented by the
-- R2 type below. Make it an instance of RIVS.
--
-- Since minus and neg have default implementations using each other, you only
-- need to provide one of them in an instance. Of course it is also harmless to
-- provide both.

data R2 = MkR2 Double Double
    deriving (Eq, Show)

instance RIVS R2 where
    zero = MkR2 0 0
    plus (MkR2 x1 x2) (MkR2 y1 y2) = MkR2 (x1 + y1) (x2 + y2)
    scale r (MkR2 x1 x2) = MkR2 (r*x1) (r*x2)
    dot (MkR2 x1 x2) (MkR2 y1 y2) = x1*y1 + x2*y2
    -- need just one of the following two
    minus (MkR2 x1 x2) (MkR2 y1 y2) = MkR2 (x1 - y1) (x2 - y2)
    neg (MkR2 x1 x2) = MkR2 (- x1) (- x2)


-- Example application: {(1,1), (2,0)} is a basis of ℝ², but not orthonormal.
-- Gram-Schmidt can turn it into an orthonormal basis:
--
--     gramschmidt [MkR2 1 1, MkR2 2 0]
--
-- gives: [MkR2 0.0 1.0,MkR2 1.0 0.0], i.e., {(0,1), (1,0)}
