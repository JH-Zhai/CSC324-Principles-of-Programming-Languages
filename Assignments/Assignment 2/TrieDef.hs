module TrieDef where

import Data.List (lookup, sortOn)
import Text.PrettyPrint as Doc hiding ((<>))

data Trie a = TrieNode (Maybe a) [(Char, Trie a)]
    deriving Show

-- handcode equality comparison to sort lists before comparing, so list order
-- doesn't matter
instance Eq a => Eq (Trie a) where
    TrieNode m1 xs1 == TrieNode m2 xs2 =
        m1 == m2
        && sortOn fst xs1 == sortOn fst xs2

-- The blank trie node for your convenience.
emptyTrie :: Trie a
emptyTrie = TrieNode Nothing []

-- Checking for blank trie node for your convenience.
isEmptyTrie :: Trie a -> Bool
isEmptyTrie (TrieNode Nothing []) = True
isEmptyTrie _ = False


-- And here are 3 operations on the list of character-labelled child nodes so
-- you can just use them and not worry about coding moar recursion yourself.

assocLookup :: Char -> [(Char, t)] -> Maybe t
assocLookup = lookup

assocDelete :: Char -> [(Char, t)] -> [(Char, t)]
assocDelete c xs = filter (\(c1, _) -> c /= c1) xs

-- This insert has replacement semantics, i.e., if the input list already has a
-- pair (c, something), the new list doesn't have that, instead has (c, new).
assocInsert :: Char -> t -> [(Char, t)] -> [(Char, t)]
assocInsert c t xs = sortOn fst ((c, t) : filter (\(c1, _) -> c /= c1) xs)


-- Nice formatting of a trie.

printTrie :: Show a => Trie a -> IO ()
printTrie = putStrLn . prettyTrie

prettyTrie :: Show a => Trie a -> String
prettyTrie trie = (render . prettyRoot) trie
  where
    prettyRoot (TrieNode mval children) =
        text "\"\"" <> prettyVal mval
        $+$
        prettyChildren children

    prettyNonRoot (c, TrieNode mval children) =
        char c <> prettyVal mval
        $+$
        prettyChildren children

    prettyVal Nothing = Doc.empty
    prettyVal (Just v) = text (" : " ++ show v)

    prettyChildren = nest 2 . vcat . map prettyNonRoot


-- My sample trie.

albertTrie :: Trie Integer
albertTrie = TrieNode (Just 4) [('a', a), ('p', p), ('t', t)]
  where
    a = TrieNode Nothing [('c', ac)]
    ac = TrieNode Nothing [('e', ace)]
    ace = TrieNode (Just 9) []
    p = TrieNode Nothing [('i', pi)]
    pi = TrieNode (Just 1) [('t', pit)]
    pit = TrieNode (Just 9) []
    t = TrieNode Nothing [('o', to)]
    to = TrieNode Nothing [('n', ton), ('p', top)]
    top = TrieNode (Just 5) []
    ton = TrieNode (Just 7) []
