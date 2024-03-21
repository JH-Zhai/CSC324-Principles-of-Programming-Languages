module TrieInsertDelete where


-- It's OK to import from the standard library. Put your import lines here.

import TrieDef

trieInsert :: [Char] -> a -> Trie a -> Trie a
trieInsert xs update (TrieNode r children)
  | null xs = TrieNode (Just update) children
  | otherwise =
      let res = assocLookup (head xs) children
      in
        case res of
          Nothing -> TrieNode r (assocInsert (head xs) (trieInsert (tail xs) update (TrieNode Nothing [])) children)
          Just res' -> TrieNode r (assocInsert (head xs) (trieInsert (tail xs) update res') children)

trieDelete :: [Char] -> Trie a -> Trie a
trieDelete xs (TrieNode r children)
  | null xs =
    if null children
      then emptyTrie
    else
      TrieNode Nothing children
  | otherwise = 
    let res = assocLookup (head xs) children
    in
      case res of
        Nothing -> TrieNode r children
        Just res' ->
          let res'' = trieDelete (tail xs) res'
          in
            if isEmptyTrie res''
              then TrieNode r (assocDelete (head xs)  children)
            else
              TrieNode r (assocInsert (head xs) res'' children)
