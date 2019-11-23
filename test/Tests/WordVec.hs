
-- | Tests for dynamic word vectors

{-# LANGUAGE CPP,BangPatterns #-}
module Tests.WordVec where

--------------------------------------------------------------------------------

import Control.Monad 

import Data.Word
import Data.Maybe
import Data.List as L

import Data.Vector.Compact.WordVec as V
import Data.Vector.Compact.Blob    as B

import Test.Tasty
import Test.Tasty.HUnit

import System.Random
import System.IO.Unsafe as Unsafe

--------------------------------------------------------------------------------

-- the fuck, we are almost writing 2020 here....
#ifdef x86_64_HOST_ARCH
arch_bits = 64 
#elif i386_HOST_ARCH
arch_bits = 32
#elif i686_HOST_ARCH
arch_bits = 32
#elif aarch64_HOST_ARCH
arch_bits = 64 
#else
arch_bits = 32
#endif

--------------------------------------------------------------------------------
-- helper functions

listMax :: [Word] -> Word
listMax [] = 0
listMax xs = L.maximum xs

listLongZipWith :: (Word -> Word -> a) -> [Word] -> [Word] -> [a]
listLongZipWith f = go where
  go (x:xs) (y:ys) = f x y : go xs ys
  go (x:xs) []     = f x 0 : go xs []
  go []     (y:ys) = f 0 y : go [] ys
  go []     []     = []

--------------------------------------------------------------------------------
-- * Naive, reference implementations
                
-- | Another implementation of 'toList', for testing purposes only
toList_extract :: WordVec -> [Word]
toList_extract dynvec@(WordVec blob)  = 
  case isSmall of
    True  -> [ B.extractSmallWord bits blob ( 8 + bits*i) | i<-[0..len-1] ]
    False -> [ B.extractSmallWord bits blob (32 + bits*i) | i<-[0..len-1] ]
  where
    (isSmall, Shape len bits) = vecShape' dynvec

tail_naive :: WordVec -> WordVec
tail_naive vec = if V.null vec
  then empty
  else fromList $ L.tail $ V.toList vec

-- | For testing purposes only
uncons_naive :: WordVec -> Maybe (Word,WordVec)
uncons_naive vec = if V.null vec 
  then Nothing
  else Just (V.head vec, tail_naive vec)

add_naive :: WordVec -> WordVec -> WordVec
add_naive vec1 vec2 = V.fromList $ listLongZipWith (+) (V.toList vec1) (V.toList vec2)

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
  , testCase "bitsNeededFor C vs. ref"           $ forall_ around_powers_of_two (\k -> bitsNeededFor  k == bitsNeededForReference  k)
  , testCase "bitsNeededFor' C vs. ref"          $ forall_ around_powers_of_two (\k -> bitsNeededFor' k == bitsNeededForReference' k)
  ]

-- [ k | k<-around_powers_of_two, bitsNeededFor' k /= bitsNeededForReference' k ]

tests_small = testGroup "unit tests for small dynamic word vectors"
  [ testGroup "conversion, basic operations (small)"
      [ testCase "toList . fromList == id"       $ forall_ small_Lists   prop_from_to_list
      , testCase "fromList . toList == id"       $ forall_ small_Vecs    prop_to_from_vec
      , testCase "fromList vs. indexing"         $ forall_ small_Lists   prop_fromlist_vs_index
      , testCase "toList vs. naive"              $ forall_ small_Vecs    prop_tolist_vs_naive
      , testCase "toRevList == reverse . toList" $ forall_ small_Vecs    prop_toRevList
      , testCase "vec head vs. list head"        $ forall_ small_NELists prop_head_of_list
      , testCase "vec last vs. list last"        $ forall_ small_NELists prop_last_of_list
      , testCase "vec tail vs. list tail"        $ forall_ small_NEVecs  prop_tail_of_list
      , testCase "tail_v1 vs. tail_v2"           $ forall_ small_Vecs    prop_tail_v1_vs_v2
      , testCase "cons_v1 vs. cons_v2"           $ forall_ small_cons    prop_cons_v1_vs_v2
      , testCase "uncons_v1 vs. uncons_v2"       $ forall_ small_Vecs    prop_uncons_v1_vs_v2
      , testCase "head vs. indexing"             $ forall_ small_NEVecs  prop_head_vs_index
      , testCase "cons . uncons == id"           $ forall_ small_NEVecs  prop_cons_uncons
      , testCase "uncons . cons == id"           $ forall_ small_cons    prop_uncons_cons
      , testCase "uncons vs. naive"              $ forall_ small_Vecs    prop_uncons_vs_naive
      , testCase "uncons vs. list"               $ forall_ small_Vecs    prop_uncons_vs_list
      , testCase "cons vs. list"                 $ forall_ small_cons    prop_cons_vs_list
      ]
  , testGroup "\"advanced\" operations (small)"
      [ testCase "sum vs. list"                  $ forall_ small_Vecs    prop_sum_vs_list
      , testCase "max vs. list"                  $ forall_ small_Vecs    prop_max_vs_list
      , testCase "strict equality vs. list"      $ forall_ small_pairs   prop_strict_eq_vs_list
      , testCase "ext0 equality vs. list"        $ forall_ small_pairs   prop_ext0_eq_vs_list
      , testCase "less or equal vs. list"        $ forall_ small_pairs   prop_less_or_equal_vs_list
      , testCase "add vs. naive"                 $ forall_ small_pairs   prop_add_vs_naive
      , testCase "add is commutative"            $ forall_ small_pairs   prop_add_commutative
      ]
  ]

tests_bighead = testGroup "unit tests for small dynamic word vectors with big heads"
  [ testGroup "conversion, basic operations (big head)"
      [ testCase "toList . fromList == id"       $ forall_ bighead_Lists   prop_from_to_list
      , testCase "fromList . toList == id"       $ forall_ bighead_Vecs    prop_to_from_vec
      , testCase "fromList vs. indexing"         $ forall_ bighead_Lists   prop_fromlist_vs_index
      , testCase "toList vs. naive"              $ forall_ bighead_Vecs    prop_tolist_vs_naive
      , testCase "toRevList == reverse . toList" $ forall_ bighead_Vecs    prop_toRevList
      , testCase "vec head vs. list head"        $ forall_ bighead_NELists prop_head_of_list
      , testCase "vec last vs. list last"        $ forall_ bighead_NELists prop_last_of_list
      , testCase "vec tail vs. list tail"        $ forall_ bighead_NEVecs  prop_tail_of_list
      , testCase "tail_v1 vs. tail_v2"           $ forall_ bighead_Vecs    prop_tail_v1_vs_v2
      , testCase "cons_v1 vs. cons_v2"           $ forall_ bighead_cons    prop_cons_v1_vs_v2
      , testCase "uncons_v1 vs. uncons_v2"       $ forall_ bighead_Vecs    prop_uncons_v1_vs_v2
      , testCase "head vs. indexing"             $ forall_ bighead_NEVecs  prop_head_vs_index
      , testCase "cons . uncons == id"           $ forall_ bighead_NEVecs  prop_cons_uncons
      , testCase "uncons . cons == id"           $ forall_ bighead_cons    prop_uncons_cons
      , testCase "uncons vs. naive"              $ forall_ bighead_Vecs    prop_uncons_vs_naive
      , testCase "uncons vs. list"               $ forall_ bighead_Vecs    prop_uncons_vs_list
      , testCase "cons vs. list"                 $ forall_ bighead_cons    prop_cons_vs_list
      ]
  , testGroup "\"advanced\" operations (big head)"
      [ testCase "sum vs. list"                  $ forall_ bighead_Vecs    prop_sum_vs_list
      , testCase "max vs. list"                  $ forall_ bighead_Vecs    prop_max_vs_list
      , testCase "strict equality vs. list"      $ forall_ bighead_pairs   prop_strict_eq_vs_list
      , testCase "ext0 equality vs. list"        $ forall_ bighead_pairs   prop_ext0_eq_vs_list
      , testCase "less or equal vs. list"        $ forall_ bighead_pairs   prop_less_or_equal_vs_list
      , testCase "add vs. naive"                 $ forall_ bighead_pairs   prop_add_vs_naive
      , testCase "add is commutative"            $ forall_ bighead_pairs   prop_add_commutative
      ]
  ]

forall_ :: [a] -> (a -> Bool) -> Assertion
forall_ xs cond = assertBool "failed" (and (map cond xs))

--------------------------------------------------------------------------------
-- * inputs

randomSublistIO :: Double -> [a] -> IO [a]
randomSublistIO prob xs = catMaybes <$> mapM f xs where
  f !x = do
    s <- randomRIO (0,1)
    if (s < prob)
      then return (Just x)
      else return Nothing

{-# NOINLINE randomSublist #-}
randomSublist :: Double -> [a] -> [a]
randomSublist prob xs = Unsafe.unsafePerformIO (randomSublistIO prob xs)
       
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

smaller_Vecs :: [Vec]
smaller_Vecs = randomSublist 0.25 small_Vecs

small_pairs :: [(Vec,Vec)]
small_pairs = [ (u,v) | u <- smaller_Vecs , v <- smaller_Vecs ]

--------------------------------------------------------------------------------

add_bighead :: List -> [List]
add_bighead (List xs) = 
  [ List (2^k-1 : xs) | k<-[1..arch_bits-1] ] ++
  [ List (2^k   : xs) | k<-[1..arch_bits-1] ] ++
  [ List (2^k+1 : xs) | k<-[1..arch_bits-1] ]
                                                       
bighead_Lists_orig = concatMap add_bighead small_Lists              :: [List]
bighead_Lists      = randomSublist 0.25 bighead_Lists_orig          :: [List]

bighead_Vecs    = [ Vec (V.fromList xs) | List xs <- bighead_Lists ]  :: [Vec]
bighead_NELists = [ NEList xs | List xs <- bighead_Lists ] :: [NEList]
bighead_NEVecs  = [ NEVec  v  | Vec  v  <- bighead_Vecs  ] :: [NEVec]

bighead_cons = [ (x, Vec (V.fromList xs)) | NEList (x:xs) <- bighead_NELists ]

smaller_bighead_Vecs :: [Vec]
smaller_bighead_Vecs = randomSublist 0.01 bighead_Vecs

bighead_pairs :: [(Vec,Vec)]
bighead_pairs = [ (u,v) | u <- smaller_bighead_Vecs , v <- smaller_bighead_Vecs ]

--------------------------------------------------------------------------------

check_eq_bitsizes = and
  [ V.fromList [k] == V.fromList' (Shape 1 b) [k] 
  | k<-[0..15] 
  , b<-[4..31]
  ]
  
around_powers_of_two :: [Word]
around_powers_of_two = [0..7] ++ stuff ++ [2^nn-i | i<-[1..8] ] where
  stuff = [ 2^i + fromIntegral ofs | i<-[2..nn-1] , ofs <- [-2..2::Int] ]
  nn = {- 64 -} arch_bits 
  
--------------------------------------------------------------------------------
-- * properties

prop_from_to_list (List list) = V.toList (V.fromList list) == list
prop_to_from_vec  (Vec  vec ) = V.fromList (V.toList vec ) == vec

prop_tolist_vs_naive (Vec vec) = (V.toList vec == toList_extract vec)
prop_toRevList       (Vec vec) = V.toRevList vec == reverse (V.toList vec)

prop_fromlist_vs_index (List list) = [ unsafeIndex i vec | i<-[0..n-1] ] == list where 
  vec = V.fromList list
  n   = V.vecLen   vec

prop_head_of_list  (NEList list) = V.head (V.fromList list) == L.head list
prop_last_of_list  (NEList list) = V.last (V.fromList list) == L.last list
prop_tail_of_list  (NEVec  vec ) = V.toList (V.tail vec) == L.tail (V.toList vec)
prop_head_vs_index (NEVec  vec ) = V.head vec == unsafeIndex 0 vec

prop_cons_uncons (NEVec vec)    =  liftM (uncurry V.cons) (V.uncons vec) == Just vec
prop_uncons_cons (w,Vec vec)    =  V.uncons (V.cons w vec) == Just (w,vec)
prop_uncons_vs_naive (Vec vec)  =  V.uncons vec == uncons_naive vec

prop_uncons_vs_list (Vec vec) = unconsToList (V.uncons vec) == L.uncons (V.toList vec)
prop_cons_vs_list (w,Vec vec) = V.toList (V.cons w vec) == w : (V.toList vec)

unconsToList mb = case mb of
  Nothing      -> Nothing
  Just (w,vec) -> Just (w, V.toList vec)

--------------------------------------------------------------------------------

prop_tail_v1_vs_v2      (Vec vec)   =  tail_v1   vec == tail_v2   vec
prop_cons_v1_vs_v2   (y,(Vec vec))  =  cons_v1 y vec == cons_v2 y vec
prop_uncons_v1_vs_v2    (Vec vec )  =  uncons_v1 vec == uncons_v2 vec

{-
bad_tail = [    vec  |    Vec vec  <- bighead_Vecs , tail_v1   vec /= tail_v2   vec ]
bad_cons = [ (y,vec) | (y,Vec vec) <- bighead_cons , cons_v1 y vec /= cons_v2 y vec ]

bad_cons_uncons = [ vec | NEVec vec <- bighead_NEVecs ,  liftM (uncurry V.cons) (V.uncons vec) /= Just vec ]
-}

--------------------------------------------------------------------------------

prop_max_vs_list (Vec vec)  =  V.maximum vec == listMax (V.toList vec)
prop_sum_vs_list (Vec vec)  =  V.sum     vec == L.sum   (V.toList vec)

{-
bad_max = [ vec | v@(Vec vec) <- bighead_Vecs , not (prop_max_vs_list v) ]
bad_sum = [ vec | v@(Vec vec) <- bighead_Vecs , not (prop_sum_vs_list v) ]
-}

--------------------------------------------------------------------------------
-- TODO: randomized tests for these
  
eqListExt0 :: [Word] -> [Word] -> Bool
eqListExt0 xs ys = and (listLongZipWith (==) xs ys)

leListExt0 :: [Word] -> [Word] -> Bool
leListExt0 xs ys = and (listLongZipWith (<=) xs ys)

prop_strict_eq_vs_list     (Vec vec1 , Vec vec2) = eqStrict    vec1 vec2 == (V.toList vec1 == V.toList vec2)
prop_ext0_eq_vs_list       (Vec vec1 , Vec vec2) = eqExtZero   vec1 vec2 == eqListExt0 (V.toList vec1) (V.toList vec2)
prop_less_or_equal_vs_list (Vec vec1 , Vec vec2) = lessOrEqual vec1 vec2 == leListExt0 (V.toList vec1) (V.toList vec2)

--------------------------------------------------------------------------------

prop_add_vs_naive    (Vec vec1 , Vec vec2) = V.add vec1 vec2 == add_naive vec1 vec2
prop_add_commutative (Vec vec1 , Vec vec2) = V.add vec1 vec2 == V.add vec2 vec1

bad_add = [ (bad1,bad2) | (Vec bad1, Vec bad2) <- bighead_pairs , not (prop_add_vs_naive (Vec bad1 , Vec bad2)) ]

--------------------------------------------------------------------------------
