module Nested where

-- You can write nested lists in some languages such as Python, e.g.,
-- [3, [1, 4, 1], 5, [9, [2, [6], 5], 3]]

-- In Haskell, the list type alone can't do this. (Why?)
-- Your mission, should you accept it, is to make nested lists possible by
-- combining the list type with a user-defined recursive data type.

-- Define the recursive data type for one item in a nested list.
-- (Not for the whole list.  The whole list is [NestedListItem a].)

data NestedListItem a = Item a | List [NestedListItem a] 
    deriving (Eq, Show)

-- such that these definitions and functions should work, for example.

-- Conceptually [4, [1, 6], 0]
example1 :: [NestedListItem Integer]
example1 = [Item 4,
            List [Item 1, Item 6],
            Item 0
           ]

flatten :: [NestedListItem a] -> [a]
flatten lst = concat (map flattenItem lst)

flattenItem :: NestedListItem a -> [a]
flattenItem (Item a) = [a]
flattenItem (List lst) = flatten lst

-- E.g., flatten example1 = [4, 1, 6, 0]

-- Now also try your hands at expressing
-- [3, [1, 4, 1], 5, [9, [2, [6], 5], 3]]
example2 :: [NestedListItem Integer]
example2 = [Item 3, List[Item 1, Item 4, Item 1], Item 5, List[Item 9, List[Item 2, List[Item 6], Item 5], Item 3]]
