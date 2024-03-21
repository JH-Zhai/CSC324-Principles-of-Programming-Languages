module TreezDef where

data BinaryTree = L | B BinaryTree BinaryTree
    deriving (Eq, Ord, Show)

-- L = leaf, B = branch. The data constructor names are kept short for shorter
-- printing outputs.

-- "OBS" stands for "ordered by size". This is a wrapper type to select the Ord
-- instance below. See below for what good this does.
data OBS t = MkOBS t
    deriving (Eq, Show)

-- Unwrapper.
unOBS :: OBS t -> t
unOBS (MkOBS a) = a

-- This class is really just for the Ord (OBS t) instance below.
class Sized t where
    size :: t -> Int  -- we expect non-neg int

-- If t is an Ord instance and also a Sized instance, then the Ord instance for
-- the OBS wrapper compares by first comparing sizes, and only when tie, by t's
-- own <=.
instance (Ord t, Sized t) => Ord (OBS t) where
    MkOBS a1 <= MkOBS a2 =
        s1 < s2
        || s1 == s2 && a1 <= a2
      where
        s1 = size a1
        s2 = size a2

-- Example: Strings are normally sorted alphabetically:
-- "abbbbbb" < "ack" < "ask"
-- If I want: Sort by length, and if two strings have the same length, then
-- alphabetical order is OK: 

instance Sized [a] where
    size xs = length xs

-- Then: MkOBS "ack" < MkOBS "ask" < MkOBS "abbbbbb".
