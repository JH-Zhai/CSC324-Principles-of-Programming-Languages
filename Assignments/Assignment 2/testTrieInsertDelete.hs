-- (Added on Nov 23)
-- How to use:
--
-- Inside ghci: main
--
-- Unix command line: runghc testTrieInsertDelete.hs

{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

import           Control.Exception (Exception, SomeException, evaluate, throwIO, try)
import           Control.Monad (when)
import           Data.Typeable (Typeable)
import           GHC.Stack (HasCallStack)
import           System.Environment (getArgs)
import           Text.Read (readMaybe)

import           TrieDef
import qualified TrieInsertDelete as C (trieInsert, trieDelete)

-- Just a lot of homebrew unit-testing code haha.

checkEq :: (HasCallStack, Eq a) => String -> a -> a -> IO ()
checkEq = checkRel (==)

checkRel :: HasCallStack => (a -> a -> Bool) -> String -> a -> a -> IO ()
checkRel rel name x y = do
    b <- evaluate (rel x y)
    when (not b) (throwIO (CheckFailed name))

data CheckException = CheckFailed String deriving (Show, Typeable)
instance Exception CheckException

dontstop action = do
    e <- try action
    case e of
      Left (exc :: SomeException) -> print exc
      Right _ -> return ()

main = do
    args <- getArgs
    case args of
      a:_ | Just n <- readMaybe a, 0 <= n, n < length tests -> tests !! n
      _ -> mapM_ dontstop tests


-- Re-assert desired types.

trieInsert :: [Char] -> a -> Trie a -> Trie a
trieInsert = C.trieInsert

trieDelete :: [Char] -> Trie a -> Trie a
trieDelete = C.trieDelete

-- The test cases.

insertTests =
    [ checkEq "insert top|->2"
               (trieInsert "top" 2 albertTrie)
               it1
    , checkEq "insert tool|->0"
               (trieInsert "tool" 0 albertTrie)
               it2
    ]

-- Expected answers for the insert tests.

it1, it2 :: Trie Integer

it1 = TrieNode (Just 4) [('a', a), ('p', p), ('t', t)]
  where
    a = TrieNode Nothing [('c', ac)]
    ac = TrieNode Nothing [('e', ace)]
    ace = TrieNode (Just 9) []
    p = TrieNode Nothing [('i', pi)]
    pi = TrieNode (Just 1) [('t', pit)]
    pit = TrieNode (Just 9) []
    t = TrieNode Nothing [('o', to)]
    to = TrieNode Nothing [('n', ton), ('p', top)]
    top = TrieNode (Just 2) []
    ton = TrieNode (Just 7) []

it2 = TrieNode (Just 4) [('a', a), ('p', p), ('t', t)]
  where
    a = TrieNode Nothing [('c', ac)]
    ac = TrieNode Nothing [('e', ace)]
    ace = TrieNode (Just 9) []
    p = TrieNode Nothing [('i', pi)]
    pi = TrieNode (Just 1) [('t', pit)]
    pit = TrieNode (Just 9) []
    t = TrieNode Nothing [('o', to)]
    to = TrieNode Nothing [('n', ton), ('o', too), ('p', top)]
    top = TrieNode (Just 5) []
    ton = TrieNode (Just 7) []
    too = TrieNode Nothing [('l', tool)]
    tool = TrieNode (Just 0) []

deleteTests =
  [ checkEq "delete pi"
            (trieDelete "pi" albertTrie)
            dt1
  , checkEq "delete ace"
            (trieDelete "ace" albertTrie)
            dt2
  , checkEq "delete foo"
            (trieDelete "foo" albertTrie)
            albertTrie
  ]

-- Expected answers for the delete tests.

dt1, dt2 :: Trie Integer

dt1 = TrieNode (Just 4) [('a', a), ('p', p), ('t', t)]
  where
    a = TrieNode Nothing [('c', ac)]
    ac = TrieNode Nothing [('e', ace)]
    ace = TrieNode (Just 9) []
    p = TrieNode Nothing [('i', pi)]
    pi = TrieNode Nothing [('t', pit)]
    pit = TrieNode (Just 9) []
    t = TrieNode Nothing [('o', to)]
    to = TrieNode Nothing [('n', ton), ('p', top)]
    top = TrieNode (Just 5) []
    ton = TrieNode (Just 7) []

dt2 = TrieNode (Just 4) [('p', p), ('t', t)]
  where
    p = TrieNode Nothing [('i', pi)]
    pi = TrieNode (Just 1) [('t', pit)]
    pit = TrieNode (Just 9) []
    t = TrieNode Nothing [('o', to)]
    to = TrieNode Nothing [('n', ton), ('p', top)]
    top = TrieNode (Just 5) []
    ton = TrieNode (Just 7) []


tests = insertTests ++ deleteTests
