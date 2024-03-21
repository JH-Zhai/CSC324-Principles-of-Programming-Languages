module Treez where

import TreezDef

-- This question develops tools for, among other applications, generating the
-- infinite list of all finite binary trees, ordered from smaller trees to
-- larger trees.
--
-- We will represent a set as a list sorted in increasing order, and each
-- element appears just once in the list. The set can be finite or infinite, so
-- the list can be finite or infinite.


-- First, You will implement some much-needed functions that work on this
-- representation.


-- Part (a) [4 marks]
-----------
--
-- Union of two sets. Since the input lists are sorted, and you're producing a
-- sorted list consisting of elements from both, this is basically the "merge"
-- in mergesort, except: If an element is in both lists,
--
-- * In merge, the output contains both occurrences.
-- * In union, the output contains just one occurrence.
--
-- Example:
--   (non-neg multiples of 5) ∪ (non-neg multiples of 3)
-- = union [0, 5..] [0, 3..]
-- = [0, 3, 5, 6, 10, 12, 15, 18, ...]
-- Note how 15 comes out just once.
--
-- This function will be useful for the next part.

union :: Ord a => [a] -> [a] -> [a]
union [] [] = []
union [] ys@(y:yt) = ys
union xs@(x:xt) [] = xs
-- union xs@(x:[]) ys@(y:yt) = insert x ys
--   where
--     insert e [] = [e]
--     insert e xs@(x:xt)
--       | e < x = e : xs
--       | e == x = xs
--       | otherwise = x : insert e xt
union xs@(x:xt) ys@(y:yt) = i : j
  where
    i =
      if x < y then x
      else y
    j =
      if x < y then union xt ys
      else if x > y then union xs yt
      else union xt yt



-- Part (b) [6 marks]
-----------
--
-- "apply2 f xs ys" computes the list that represents this set:
-- { f a b | a ∈ xs, b ∈ ys }
-- We assume that f is strictly increasing, i.e.,
-- * if x1 < x2, then f x1 y < f x2 y
-- * if y1 < y2, then f x y1 < f x y2
--
-- This is like the cartesian product of two sets but then you apply f to every
-- pair.
--
-- Example: All positive integers of the form 2^i * 3^j (natural i and j) in
-- increasing order:
--
--   {a*b | a is a power of 2, b is a power of 3}
-- = apply2 (*) (iterate (\a -> a*2) 1) (iterate (\b -> b*3) 1)
-- = [1,2,3,4,6,8,9,12,16,18,24,27,32,36,48,54,64,72,81,...]
--
-- How not to do it: Clearly, the simple list comprehension
--     [ f a b | a <- xs, b <- ys ]
-- Is wrong: Wrong order, and has extra problems in the infinite case.
--
-- How to do it: The two base cases are obvious. For the recursive case:
--
-- Suppose xs = x:xt, so x is the smallest element there, xt are the rest.
-- Suppose ys = y:yt, so y is the smallest element there, yt are the rest.
-- You are doing {f a b | a ∈ x:xt, b ∈ y:yt}
-- Cool picture: Draw a table, on one axis are elements of xs, on the other axis
-- are elements of ys, the cells are the "f a b"s.
--
-- You can use a recursive call to cover {f a b | a ∈ xt, b ∈ yt}
-- In fact, you can use the recursive call to cover a bit more, e.g.,
-- {f a b | a ∈ xt, b ∈ y:yt}
-- In the picture, which region does the recursive call cover?
-- So, how do you code up the rest? What is the rest?
--
-- Another reminder: f x y is definitely the smallest element in the answer,
-- since x is smallest in xs, y is smallest in ys, and f maps smaller elements
-- to smaller elements.

apply2 :: (Ord a, Ord b, Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
apply2 f [] [] = []
apply2 f _ [] = []
apply2 f [] _ = []
apply2 f xs@(x:xt) ys@(y:yt) = i : j
  where
    i = f x y
    j = union (apply2 f xt ys) (apply2 f xs yt)



-- Now about the binary trees.
--
-- TreezDef.hs has the binary tree type BinaryTree used in this question.
--
-- It has the auto-generated sorting order ("deriving Ord").  However, we want
-- to sort by tree size first, and then among trees of the same size, it's OK to
-- use the auto-generated order.
--
-- This is how the OBS type in TreezDef.hs can help. If we wrap our binary tree
-- inside MkOBS, and if we make BinaryTree an instance of Sized (again in
-- TreezDef.hs), then OBS comparison uses the size method first, then the
-- auto-generated order, as wanted.

-- Part (c) [2 marks]
-----------
--
-- Implement the size method for BinaryTree.  A straightforward recursion will
-- do, doesn't have to be fancy.  For this question, we simply count the total
-- number of branch nodes (B).
--
-- Example: size (B (B L L) L) = 2
-- because it contains 2 branch nodes.

instance Sized BinaryTree where
  size L = 0
  size (B l r) = 1 + size l + size r



-- Part (d) [4 marks]
-----------
--
-- Test of faith in recursive data equations and lazy lists. :)
--
-- The set S of all finite binary trees satisfies this recursive equation (and
-- is the smallest set that does):
--
--     S = {L} ∪ {B lt rt | lt ∈ S, rt ∈ S}
--
-- Since we use sorted lists for sets, and since Leaf is clearly the smallest
-- element and should be first, it looks like
--
--     S = L : {B lt rt | lt ∈ S, rt ∈ S}
--
-- and we know that apply2 can help with the {B lt rt | lt ∈ S, rt ∈ S} part.
--
-- But wait! You need the OBS/MkOBS wrapper so apply2 uses the correct order!
-- How to adjust for that?
--
-- Implement obstreez below for the infinite list of binary trees, each tree
-- wrapped inside MkOBS, so that smaller trees are ordered first by apply2.
--
-- Then treez below just needs to unwrap by unOBS to give you the list of the
-- trees themselves.

obstreez :: [OBS BinaryTree]
obstreez = MkOBS L : apply2 (\x y -> MkOBS (B (unOBS x)  (unOBS y))) obstreez obstreez

          
treez :: [BinaryTree]
treez = map unOBS obstreez

-- Example: take 9 treez =
--   [L,                                                        -- size 0
--    B L L,                                                    -- size 1
--    B L (B L L), B (B L L) L,                                 -- size 2
--    B L (B L (B L L)), B L (B (B L L) L), B (B L L) (B L L),  -- size 3
--    B (B L (B L L)) L, B (B (B L L) L) L                      -- size 3 cont.
--   ]
