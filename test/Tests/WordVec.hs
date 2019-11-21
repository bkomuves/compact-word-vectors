
-- | Tests for dynamic word vectors

{-# LANGUAGE CPP,BangPatterns #-}
module Tests.WordVec where

--------------------------------------------------------------------------------

import Control.Monad ( liftM )

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
  [ tests_unit
  , tests_small
  , tests_bighead
  ]

tests_unit = testGroup "misc unit tests"
  [ testCase "equality with different bit sizes" $ assertBool "failed" $ check_eq_bitsizes
  , testCase "head of empty == 0"                $ assertBool "failed" $ (V.head V.empty == 0)
  , testCase "last of empty == 0"                $ assertBool "failed" $ (V.last V.empty == 0)
  , testCase "tail of empty == empty"            $ assertBool "failed" $ (V.tail V.empty == V.empty)  
  ]

tests_small = testGroup "unit tests for small dynamic word vectors"
  [ testCase "toList . fromList == id"       $ forall_ small_Lists   prop_from_to_list
  , testCase "fromList . toList == id"       $ forall_ small_Vecs    prop_to_from_vec
  , testCase "fromList vs. indexing"         $ forall_ small_Lists   prop_fromlist_vs_index
  , testCase "toList vs. naive"              $ forall_ small_Vecs    prop_tolist_vs_naive
  , testCase "toRevList == reverse . toList" $ forall_ small_Vecs    prop_toRevList
  , testCase "vec head vs. list head"        $ forall_ small_NELists prop_head_of_list
  , testCase "vec last vs. list last"        $ forall_ small_NELists prop_last_of_list
  , testCase "head vs. indexing"             $ forall_ small_NEVecs  prop_head_vs_index
  , testCase "cons . uncons == id"           $ forall_ small_NEVecs  prop_cons_uncons
  , testCase "uncons . cons == id"           $ forall_ small_cons    prop_uncons_cons
  , testCase "uncons vs. naive"              $ forall_ small_Vecs    prop_uncons_vs_naive
  , testCase "uncons vs. list"               $ forall_ small_Vecs    prop_uncons_vs_list
  , testCase "cons vs. list"                 $ forall_ small_cons    prop_cons_vs_list
  ]

tests_bighead = testGroup "unit tests for small dynamic word vectors with big heads"
  [ testCase "toList . fromList == id"       $ forall_ bighead_Lists   prop_from_to_list
  , testCase "fromList . toList == id"       $ forall_ bighead_Vecs    prop_to_from_vec
  , testCase "fromList vs. indexing"         $ forall_ bighead_Lists   prop_fromlist_vs_index
  , testCase "toList vs. naive"              $ forall_ bighead_Vecs    prop_tolist_vs_naive
  , testCase "toRevList == reverse . toList" $ forall_ bighead_Vecs    prop_toRevList
  , testCase "vec head vs. list head"        $ forall_ bighead_NELists prop_head_of_list
  , testCase "vec last vs. list last"        $ forall_ bighead_NELists prop_last_of_list
  , testCase "head vs. indexing"             $ forall_ bighead_NEVecs  prop_head_vs_index
  , testCase "cons . uncons == id"           $ forall_ bighead_NEVecs  prop_cons_uncons
  , testCase "uncons . cons == id"           $ forall_ bighead_cons    prop_uncons_cons
  , testCase "uncons vs. naive"              $ forall_ bighead_Vecs    prop_uncons_vs_naive
  , testCase "uncons vs. list"               $ forall_ bighead_Vecs    prop_uncons_vs_list
  , testCase "cons vs. list"                 $ forall_ bighead_cons    prop_cons_vs_list
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

small_cons :: [(Word,Vec)] 
small_cons = [ (x, Vec (V.fromList xs)) | NEList (x:xs) <- small_NELists ]

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

bighead_cons = [ (x, Vec (V.fromList xs)) | NEList (x:xs) <- bighead_NELists ]

--------------------------------------------------------------------------------

check_eq_bitsizes = and
  [ V.fromList [k] == V.fromList' (Shape 1 b) [k] 
  | k<-[0..15] 
  , b<-[4..31]
  ]

--------------------------------------------------------------------------------
-- * properties

prop_from_to_list (List list) = V.toList (V.fromList list) == list
prop_to_from_vec  (Vec  vec ) = V.fromList (V.toList vec ) == vec

prop_tolist_vs_naive (Vec vec) = (V.toList vec == V.toList_naive vec)
prop_toRevList       (Vec vec) = V.toRevList vec == reverse (V.toList vec)

prop_fromlist_vs_index (List list) = [ unsafeIndex i vec | i<-[0..n-1] ] == list where 
  vec = V.fromList list
  n   = V.vecLen   vec

prop_head_of_list  (NEList list) = V.head (V.fromList list) == L.head list
prop_last_of_list  (NEList list) = V.last (V.fromList list) == L.last list
prop_head_vs_index (NEVec  vec ) = V.head vec == unsafeIndex 0 vec

prop_cons_uncons (NEVec vec)    =  liftM (uncurry V.cons) (V.uncons vec) == Just vec
prop_uncons_cons (w,Vec vec)    =  V.uncons (V.cons w vec) == Just (w,vec)
prop_uncons_vs_naive (Vec vec)  =  V.uncons vec == V.uncons_naive vec

prop_uncons_vs_list (Vec vec) = unconsToList (V.uncons vec) == L.uncons (V.toList vec)
prop_cons_vs_list (w,Vec vec) = V.toList (V.cons w vec) == w : (V.toList vec)

unconsToList mb = case mb of
  Nothing      -> Nothing
  Just (w,vec) -> Just (w, V.toList vec)

--------------------------------------------------------------------------------
