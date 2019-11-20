
-- | Tests for dynamic word vectors

{-# LANGUAGE CPP,BangPatterns #-}
module Tests.WordVec where

--------------------------------------------------------------------------------

import Data.Word
import Data.List as L

import Data.Vector.Compact.WordVec as V

import Test.Tasty
import Test.Tasty.HUnit

#ifdef x86_64_HOST_ARCH
arch_bits = 64 
#elif i386_HOST_ARCH
arch_bits = 32
#else
arch_bits = 32
#endif

--------------------------------------------------------------------------------

all_tests = testGroup "unit tests for WordVec-s"
  [ tests_small
  , tests_bighead
  ]

tests_small = testGroup "unit tests for small dynamic word vectors"
  [ testCase "toList . fromList == id"    $ forall_ small_Lists   prop_from_to_list
  , testCase "fromList . toList == id"    $ forall_ small_Vecs    prop_to_from_vec
  , testCase "fromList vs. indexing"      $ forall_ small_Lists   prop_fromlist_vs_index
  , testCase "vec head vs. list head"     $ forall_ small_NELists prop_head_of_list
  , testCase "head vs. indexing"          $ forall_ small_NEVecs  prop_head_vs_index
  ]

tests_bighead = testGroup "unit tests for small dynamic word vectors with big heads"
  [ testCase "toList . fromList == id"    $ forall_ bighead_Lists   prop_from_to_list
  , testCase "fromList . toList == id"    $ forall_ bighead_Vecs    prop_to_from_vec
  , testCase "fromList vs. indexing"      $ forall_ bighead_Lists   prop_fromlist_vs_index
  , testCase "vec head vs. list head"     $ forall_ bighead_NELists prop_head_of_list
  , testCase "head vs. indexing"          $ forall_ bighead_NEVecs  prop_head_vs_index
  ]

forall_ :: [a] -> (a -> Bool) -> Assertion
forall_ xs cond = assertBool "failed" (and (map cond xs))

--------------------------------------------------------------------------------
-- * inputs
newtype List   = List   [Word]  deriving Show
newtype NEList = NEList [Word]  deriving Show

newtype Vec   = Vec   WordVec  deriving Show
newtype NEVec = NEVec WordVec  deriving Show

small_Lists :: [List]
small_Lists = List [] : [ List xs | NEList xs <- small_NELists ]

small_NELists :: [NEList]
small_NELists = [ NEList [ofs..ofs+len-1] | ofs<-[0..25] , len<-[1..65] ]

small_Vecs :: [Vec]
small_Vecs = [ Vec (V.fromList xs) | List xs <- small_Lists ]

small_NEVecs :: [NEVec]
small_NEVecs = [ NEVec (V.fromList xs) | NEList xs <- small_NELists ]

--------------------------------------------------------------------------------

add_bighead :: List -> [List]
add_bighead (List xs) = 
  [ List (2^k-1 : xs) | k<-[1..arch_bits-1] ] ++
  [ List (2^k   : xs) | k<-[1..arch_bits-1] ] ++
  [ List (2^k+1 : xs) | k<-[1..arch_bits-1] ]
                                                       
bighead_Lists = concatMap add_bighead small_Lists                   :: [List]
bighead_Vecs  = [ Vec (V.fromList xs) | List xs <- bighead_Lists ]  :: [Vec]
bighead_NELists = [ NEList xs | List xs <- bighead_Lists ] :: [NEList]
bighead_NEVecs  = [ NEVec  v  | Vec  v  <- bighead_Vecs  ] :: [NEVec]

--------------------------------------------------------------------------------
-- * properties

prop_from_to_list (List list) = V.toList (V.fromList list) == list
prop_to_from_vec  (Vec  vec ) = V.fromList (V.toList vec ) == vec

prop_fromlist_vs_index (List list) = [ unsafeIndex i vec | i<-[0..n-1] ] == list where 
  vec = V.fromList list
  n   = V.vecLen   vec

prop_head_of_list  (NEList list) = V.head (V.fromList list) == L.head list
prop_head_vs_index (NEVec  vec ) = V.head vec == unsafeIndex 0 vec

--------------------------------------------------------------------------------
