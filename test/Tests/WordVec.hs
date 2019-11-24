
-- | Tests for dynamic word vectors

{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
module Tests.WordVec where

--------------------------------------------------------------------------------

import Control.Monad 

import Data.Word
import Data.Bits
import Data.Maybe
import Data.List as L

import Data.Vector.Compact.WordVec as V
import Data.Vector.Compact.Blob    as B

import Foreign.C.Types

import Test.Tasty
import Test.Tasty.HUnit

import System.Random
import System.IO.Unsafe as Unsafe

--------------------------------------------------------------------------------


-- the fuck, we are almost writing 2020 here, why am i doing this?!....
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

cmpStrict_naive :: WordVec -> WordVec -> Ordering
cmpStrict_naive x y = case compare (vecLen x) (vecLen y) of 
  LT -> LT
  GT -> GT
  EQ -> compare (toList x) (toList y)

cmpExtZero_naive :: WordVec -> WordVec -> Ordering
cmpExtZero_naive x y = go (toList x) (toList y) where
  go (x:xs) (y:ys) = case compare x y of
    LT -> LT
    GT -> GT 
    EQ -> go xs ys
  go (x:xs) []     = go (x:xs) [0] 
  go []     (y:ys) = go [0]    (y:ys)  
  go []     []     = EQ
  
--------------------------------------------------------------------------------

bitsNeededForHs :: Word -> Int
bitsNeededForHs = roundBits . bitsNeededForHs'

bitsNeededForHs' :: Word -> Int
bitsNeededForHs' bound 
  | bound   == 0  = 1                                 -- this is handled incorrectly by the formula below
  | bound+1 == 0  = arch_bits -- MACHINE_WORD_BITS    -- and this handled incorrectly because of overflow
  | otherwise     = ceilingLog2 (bound + 1)           -- for example, if maximum is 16, log2 = 4 but we need 5 bits 
  where    
    -- | Smallest integer @k@ such that @2^k@ is larger or equal to @n@
    ceilingLog2 :: Word -> Int
    ceilingLog2 0 = 0
    ceilingLog2 n = 1 + go (n-1) where
      go 0 = -1
      go k = 1 + go (shiftR k 1)

-- apparently, the C implementation is _not_ faster...
bitsNeededForC :: Word -> Int
bitsNeededForC = fromIntegral . export_required_bits . fromIntegral

bitsNeededForC' :: Word -> Int
bitsNeededForC' = fromIntegral . export_required_bits_not_rounded . fromIntegral

foreign import ccall unsafe "export_required_bits_not_rounded" export_required_bits_not_rounded :: Word64 -> CInt
foreign import ccall unsafe "export_required_bits"             export_required_bits             :: Word64 -> CInt

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

--------------------------------------------------------------------------------

tail_v1 :: WordVec -> WordVec
tail_v1 dynvec 
  | len == 0   = empty
  | otherwise  = fromList' (Shape (len-1) bits) (L.tail $ toList dynvec)
  where
    (Shape len bits) = vecShape dynvec

uncons_v1 :: WordVec -> Maybe (Word,WordVec)
uncons_v1 vec  
  | len == 0   = Nothing
  | otherwise  = Just $ case toList vec of { (w:ws) -> (w , fromList' (Shape (len-1) bits) ws) }
  where
    (Shape len bits) = vecShape vec

cons_v1 :: Word -> WordVec -> WordVec
cons_v1 w vec = fromList' shape' (w : toList vec) where
  (Shape len bits) = vecShape vec
  bits'  = max bits (bitsNeededFor w)
  shape' = Shape (len+1) bits'

snoc_v1 :: WordVec -> Word -> WordVec
snoc_v1 vec w = fromList' shape' (toList vec ++ [w]) where
  (Shape len bits) = vecShape vec
  bits'  = max bits (bitsNeededFor w)
  shape' = Shape (len+1) bits'

tail_v2   = V.tail
cons_v2   = V.cons
snoc_v2   = V.snoc
uncons_v2 = V.uncons

--------------------------------------------------------------------------------

null_naive :: WordVec -> Bool
null_naive v = (vecLen v == 0)

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

sub_naive :: WordVec -> WordVec -> Maybe WordVec
sub_naive vec1 vec2 = case and (listLongZipWith (>=) (V.toList vec1) (V.toList vec2)) of
  True  -> Just $ V.fromList $  listLongZipWith (-)  (V.toList vec1) (V.toList vec2)
  False -> Nothing

scale_naive :: Word -> WordVec -> WordVec
scale_naive s = V.fromList . L.map (*s) . V.toList 

partialSums_naive :: WordVec -> WordVec
partialSums_naive = V.fromList . L.tail . L.scanl' (+) 0 . V.toList

--------------------------------------------------------------------------------

all_tests = testGroup "tests for WordVec-s"
  [ tests_unit
  , tests_small
  , tests_rnd
  , tests_bighead
  ]

tests_unit = testGroup "misc unit tests"
  [ testCase "equality with different bit sizes"   $ assertBool "failed" $ check_eq_bitsizes
  , testCase "head of empty == 0"                  $ assertBool "failed" $ (V.head V.empty == 0)
  , testCase "last of empty == 0"                  $ assertBool "failed" $ (V.last V.empty == 0)
  , testCase "tail of empty == empty"              $ assertBool "failed" $ (V.tail V.empty == V.empty)  
  , testCase "bitsNeededFor C vs. ref"             $ forall_ around_powers_of_two (\k -> bitsNeededForHs  k == bitsNeededForC  k)
  , testCase "bitsNeededFor' C vs. ref"            $ forall_ around_powers_of_two (\k -> bitsNeededForHs' k == bitsNeededForC' k)
  , testCase "cons_v2 crash (left-shift) is fixed" $ assertBool "failed" $ cons_v2_crash
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
      , testCase "snoc_v1 vs. snoc_v2"           $ forall_ small_cons    prop_snoc_v1_vs_v2
      , testCase "uncons_v1 vs. uncons_v2"       $ forall_ small_Vecs    prop_uncons_v1_vs_v2
      , testCase "head vs. indexing"             $ forall_ small_NEVecs  prop_head_vs_index
      , testCase "cons . uncons == id"           $ forall_ small_NEVecs  prop_cons_uncons
      , testCase "uncons . cons == id"           $ forall_ small_cons    prop_uncons_cons
      , testCase "uncons vs. naive"              $ forall_ small_Vecs    prop_uncons_vs_naive
      , testCase "uncons vs. list"               $ forall_ small_Vecs    prop_uncons_vs_list
      , testCase "cons vs. list"                 $ forall_ small_cons    prop_cons_vs_list
      , testCase "snoc vs. list"                 $ forall_ small_cons    prop_snoc_vs_list
      ]
  , testGroup "\"advanced\" operations (small)"
      [ testCase "sum vs. list"                  $ forall_ small_Vecs    prop_sum_vs_list
      , testCase "max vs. list"                  $ forall_ small_Vecs    prop_max_vs_list
      , testCase "strict equality vs. list"      $ forall_ small_pairs   prop_strict_eq_vs_list
      , testCase "strict comparison vs. list"    $ forall_ small_pairs   prop_strict_cmp_vs_list
      , testCase "ext0 equality vs. list"        $ forall_ small_pairs   prop_ext0_eq_vs_list
      , testCase "ext0 comparison vs. list"      $ forall_ small_pairs   prop_ext0_cmp_vs_list
      , testCase "less or equal vs. list"        $ forall_ small_pairs   prop_less_or_equal_vs_list
      , testCase "add vs. naive"                 $ forall_ small_pairs   prop_add_vs_naive
      , testCase "sub vs. naive"                 $ forall_ small_pairs   prop_sub_vs_naive
      , testCase "add is commutative"            $ forall_ small_pairs   prop_add_commutative
      , testCase "partial sums vs. naive"        $ forall_ small_Vecs    prop_psums_vs_naive
      , testCase "scale 3 vs. naive"             $ forall_ small_Vecs   (prop_scale_vs_naive 3)
      , testCase "scale 14 vs. naive"            $ forall_ small_Vecs   (prop_scale_vs_naive 14)
      , testCase "scale 254 vs. naive"           $ forall_ small_Vecs   (prop_scale_vs_naive 254)
      , testCase "scale 65000 vs. naive"         $ forall_ small_Vecs   (prop_scale_vs_naive 65000)
      ]
  ]

tests_rnd = testGroup "tests for random dynamic word vectors"
  [ testGroup "conversion, basic operations (random)"
      [ testCase "toList . fromList == id"       $ forall_ rnd_Lists   prop_from_to_list
      , testCase "fromList . toList == id"       $ forall_ rnd_Vecs    prop_to_from_vec
      , testCase "fromList vs. indexing"         $ forall_ rnd_Lists   prop_fromlist_vs_index
      , testCase "toList vs. naive"              $ forall_ rnd_Vecs    prop_tolist_vs_naive
      , testCase "toRevList == reverse . toList" $ forall_ rnd_Vecs    prop_toRevList
      , testCase "vec head vs. list head"        $ forall_ rnd_NELists prop_head_of_list
      , testCase "vec last vs. list last"        $ forall_ rnd_NELists prop_last_of_list
      , testCase "vec tail vs. list tail"        $ forall_ rnd_NEVecs  prop_tail_of_list
      , testCase "tail_v1 vs. tail_v2"           $ forall_ rnd_Vecs    prop_tail_v1_vs_v2
      , testCase "cons_v1 vs. cons_v2"           $ forall_ rnd_cons    prop_cons_v1_vs_v2
      , testCase "snoc_v1 vs. snoc_v2"           $ forall_ rnd_cons    prop_snoc_v1_vs_v2
      , testCase "uncons_v1 vs. uncons_v2"       $ forall_ rnd_Vecs    prop_uncons_v1_vs_v2
      , testCase "head vs. indexing"             $ forall_ rnd_NEVecs  prop_head_vs_index
      , testCase "cons . uncons == id"           $ forall_ rnd_NEVecs  prop_cons_uncons
      , testCase "uncons . cons == id"           $ forall_ rnd_cons    prop_uncons_cons
      , testCase "uncons vs. naive"              $ forall_ rnd_Vecs    prop_uncons_vs_naive
      , testCase "uncons vs. list"               $ forall_ rnd_Vecs    prop_uncons_vs_list
      , testCase "cons vs. list"                 $ forall_ rnd_cons    prop_cons_vs_list
      , testCase "snoc vs. list"                 $ forall_ rnd_cons    prop_snoc_vs_list
      ]
  , testGroup "\"advanced\" operations (random)"
      [ testCase "sum vs. list"                  $ forall_ rnd_Vecs    prop_sum_vs_list
      , testCase "max vs. list"                  $ forall_ rnd_Vecs    prop_max_vs_list
      , testCase "strict equality vs. list"      $ forall_ rnd_pairs   prop_strict_eq_vs_list
      , testCase "strict comparison vs. list"    $ forall_ rnd_pairs   prop_strict_cmp_vs_list
      , testCase "ext0 equality vs. list"        $ forall_ rnd_pairs   prop_ext0_eq_vs_list
      , testCase "ext0 comparison vs. list  "    $ forall_ rnd_pairs   prop_ext0_cmp_vs_list
      , testCase "less or equal vs. list"        $ forall_ rnd_pairs   prop_less_or_equal_vs_list
      , testCase "add vs. naive"                 $ forall_ rnd_pairs   prop_add_vs_naive
      , testCase "sub vs. naive"                 $ forall_ rnd_pairs   prop_sub_vs_naive
      , testCase "add is commutative"            $ forall_ rnd_pairs   prop_add_commutative
      , testCase "partial sums vs. naive"        $ forall_ rnd_Vecs    prop_psums_vs_naive
      , testCase "scale 3 vs. naive"             $ forall_ rnd_Vecs   (prop_scale_vs_naive 3)
      , testCase "scale 14 vs. naive"            $ forall_ rnd_Vecs   (prop_scale_vs_naive 14)
      , testCase "scale 254 vs. naive"           $ forall_ rnd_Vecs   (prop_scale_vs_naive 254)
      , testCase "scale 65000 vs. naive"         $ forall_ rnd_Vecs   (prop_scale_vs_naive 65000)
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
      , testCase "snoc_v1 vs. snoc_v2"           $ forall_ bighead_cons    prop_snoc_v1_vs_v2
      , testCase "uncons_v1 vs. uncons_v2"       $ forall_ bighead_Vecs    prop_uncons_v1_vs_v2
      , testCase "head vs. indexing"             $ forall_ bighead_NEVecs  prop_head_vs_index
      , testCase "cons . uncons == id"           $ forall_ bighead_NEVecs  prop_cons_uncons
      , testCase "uncons . cons == id"           $ forall_ bighead_cons    prop_uncons_cons
      , testCase "uncons vs. naive"              $ forall_ bighead_Vecs    prop_uncons_vs_naive
      , testCase "uncons vs. list"               $ forall_ bighead_Vecs    prop_uncons_vs_list
      , testCase "cons vs. list"                 $ forall_ bighead_cons    prop_cons_vs_list
      , testCase "snoc vs. list"                 $ forall_ bighead_cons    prop_snoc_vs_list
      ]
  , testGroup "\"advanced\" operations (big head)"
      [ testCase "sum vs. list"                  $ forall_ bighead_Vecs    prop_sum_vs_list
      , testCase "max vs. list"                  $ forall_ bighead_Vecs    prop_max_vs_list
      , testCase "strict equality vs. list"      $ forall_ bighead_pairs   prop_strict_eq_vs_list
      , testCase "strict comparison vs. list"    $ forall_ bighead_pairs   prop_strict_cmp_vs_list
      , testCase "ext0 equality vs. list"        $ forall_ bighead_pairs   prop_ext0_eq_vs_list
      , testCase "ext0 comparison vs. list  "    $ forall_ bighead_pairs   prop_ext0_cmp_vs_list
      , testCase "less or equal vs. list"        $ forall_ bighead_pairs   prop_less_or_equal_vs_list
      , testCase "add vs. naive"                 $ forall_ bighead_pairs   prop_add_vs_naive
      , testCase "sub vs. naive"                 $ forall_ bighead_pairs   prop_sub_vs_naive
      , testCase "add is commutative"            $ forall_ bighead_pairs   prop_add_commutative
      , testCase "partial sums vs. naive"        $ forall_ bighead_Vecs    prop_psums_vs_naive
      , testCase "scale 3 vs. naive"             $ forall_ bighead_Vecs   (prop_scale_vs_naive 3)
      , testCase "scale 14 vs. naive"            $ forall_ bighead_Vecs   (prop_scale_vs_naive 14)
      , testCase "scale 254 vs. naive"           $ forall_ bighead_Vecs   (prop_scale_vs_naive 254)
      , testCase "scale 65000 vs. naive"         $ forall_ bighead_Vecs   (prop_scale_vs_naive 65000)
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

randomVec :: Int -> Int -> IO Vec
randomVec maxlen maxbits = do
  len  <- randomRIO (0,maxlen )
  bits <- randomRIO (0,maxbits)
  let bnd = 2^bits - 1 
  xs <- replicateM len (randomRIO (0,bnd))
  -- print (bnd,xs)
  return $ Vec $ V.fromListN len bnd xs
  
{-# NOINLINE rnd_Vecs #-}
rnd_Vecs :: [Vec]
rnd_Vecs = L.concat $ L.concat $ Unsafe.unsafePerformIO $ do
  forM [1,5..100] $ \len -> do
    forM [4,8..64] $ \maxbits -> do
      let k = maxbits  -- for smaller bit depths, we need less test cases
      replicateM k (randomVec len maxbits)
      
rnd_Lists   = [ List (V.toList vec) | Vec vec <- rnd_Vecs ]

rnd_NELists = [ NEList l | List l <- rnd_Lists , not (L.null l) ]
rnd_NEVecs  = [ NEVec  v | Vec  v <- rnd_Vecs  , not (V.null v) ]
   
rnd_cons = [ (x, Vec (V.fromList xs)) | NEList (x:xs) <- rnd_NELists ]

smaller_rnd_Vecs :: [Vec]
smaller_rnd_Vecs = randomSublist 0.025 rnd_Vecs
   
rnd_pairs :: [(Vec,Vec)]
rnd_pairs = [ (u,v) | u <- smaller_rnd_Vecs , v <- smaller_rnd_Vecs ]
   
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
-- * misc tests

-- cons fatal error (caused by non-strict left shift of blobs):
cons_v2_crash = V.toList (cons_v2 x xs) == [159407,244557,137175,96511,42979] where
  (x, Vec xs) = (159407, Vec (fromList' (Shape {shapeLen = 4, shapeBits = 20}) [244557,137175,96511,42979]))
  
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
prop_snoc_vs_list (w,Vec vec) = V.toList (V.snoc vec w) == (V.toList vec) ++ [w]

unconsToList mb = case mb of
  Nothing      -> Nothing
  Just (w,vec) -> Just (w, V.toList vec)

--------------------------------------------------------------------------------

prop_tail_v1_vs_v2      (Vec vec)   =  tail_v1   vec == tail_v2   vec
prop_cons_v1_vs_v2   (y,(Vec vec))  =  cons_v1 y vec == cons_v2 y vec
prop_snoc_v1_vs_v2   (y,(Vec vec))  =  snoc_v1 vec y == snoc_v2 vec y 
prop_uncons_v1_vs_v2    (Vec vec )  =  uncons_v1 vec == uncons_v2 vec

{-
bad_tail = [    vec  |    Vec vec  <- bighead_Vecs , tail_v1   vec /= tail_v2   vec ]
bad_cons = [ (y,vec) | (y,Vec vec) <- bighead_cons , cons_v1 y vec /= cons_v2 y vec ]

bad_cons_uncons = [ vec | NEVec vec <- bighead_NEVecs ,  liftM (uncurry V.cons) (V.uncons vec) /= Just vec ]
-}

bad_snoc = [ (y,vec) | (y,Vec vec) <- small_cons   , snoc_v1 vec y /= snoc_v2 vec y ]

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
prop_strict_cmp_vs_list    (Vec vec1 , Vec vec2) = cmpStrict   vec1 vec2 == (vec1 `cmpStrict_naive` vec2)
prop_ext0_eq_vs_list       (Vec vec1 , Vec vec2) = eqExtZero   vec1 vec2 == eqListExt0 (V.toList vec1) (V.toList vec2)
prop_ext0_cmp_vs_list      (Vec vec1 , Vec vec2) = cmpExtZero  vec1 vec2 == (vec1 `cmpExtZero_naive` vec2)
prop_less_or_equal_vs_list (Vec vec1 , Vec vec2) = lessOrEqual vec1 vec2 == leListExt0 (V.toList vec1) (V.toList vec2)

--------------------------------------------------------------------------------

prop_add_commutative (Vec vec1 , Vec vec2) = V.add      vec1 vec2 == V.add vec2 vec1
prop_add_vs_naive    (Vec vec1 , Vec vec2) = V.add      vec1 vec2 == add_naive vec1 vec2
prop_sub_vs_naive    (Vec vec1 , Vec vec2) = V.subtract vec1 vec2 == sub_naive vec1 vec2

-- bad_add = [ (bad1,bad2) | (Vec bad1, Vec bad2) <- bighead_pairs , not (prop_add_vs_naive (Vec bad1 , Vec bad2)) ]

prop_psums_vs_naive   (Vec vec) = V.partialSums vec == partialSums_naive vec
prop_scale_vs_naive s (Vec vec) = V.scale s vec     == scale_naive s vec

bad_psums  = [ bad | Vec bad <- bighead_Vecs , not (prop_psums_vs_naive   (Vec bad)) ]
bad_scale3 = [ bad | Vec bad <- bighead_Vecs , not (prop_scale_vs_naive 3 (Vec bad)) ]

--------------------------------------------------------------------------------
