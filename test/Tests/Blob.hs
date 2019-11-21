
-- | Tests for blobs

{-# LANGUAGE CPP,BangPatterns #-}
module Tests.Blob where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word
import Data.List as L

import Data.Primitive.ByteArray

import Data.Vector.Compact.Blob

import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------

all_tests = tests_blobs

tests_blobs = testGroup "unit tests for Blobs"
  [ testCase "toList . fromList == id"     $ forall_ w64_lists   prop_from_to_list
  , testCase "fromList . toList == id"     $ forall_ blobs       prop_to_from_blob
  , testCase "toList vs. peekWord"         $ forall_ blobs       prop_tolist_vs_peek
  , testCase "blobHead vs. list head"      $ forall_ w64_lists   prop_head_of_list
  , testCase "eqBlob vs. naive"            $ forall_ blob_pairs  prop_eq_vs_naive
  , testCase "fromBA . toBA == id"         $ forall_ blobs       prop_to_from_bytearray
  , testCase "toBA . fromBA == pad"        $ forall_ byte_lists  prop_from_to_bytearray
    -- 
  , testCase "tail . cons == id"           $ forall_ blobs       prop_tail_cons
  , testCase "shiftR_64 . shiftL_64 == id" $ forall_ blobs       prop_shiftR64_shiftL64
  , testCase "shiftL_64 . shiftR_64 = ..." $ forall_ blobs       prop_shiftL64_shiftR64
  , testCase "shiftR . shiftL ~= id"       $ forall_ blobs       prop_shiftR_shiftL
  ]

forall_ :: [a] -> (a -> Bool) -> Assertion
forall_ xs cond = assertBool "failed" (and (map cond xs))

--------------------------------------------------------------------------------
-- * inputs

rndWords1, rndWords2 :: [Word64]

rndWords1 = 
  [0x636bab7ce12a9f82,0xacb1651d835dea8a,0x53ee90f119788b9c,0xb3e5f34f1a7b14df,0x108afb5d5ef5420b
  ,0xad8a4492183b5890,0x29338ff97c3c4b30,0xab38a94da16f8831,0x3eebedc58090704f,0xfb0de19c60305cfc
  ,0xe1f9cc292fd9fd99,0x661cbeb165fc5369,0x45b9557954fa8197,0x5999769d181a0e58,0xbbbc1a04bf66f1a7
  ,0x8b14a985d9e28575,0xa59e5b55e5bd190f,0xc86dcfa51ec3f984,0x8ada7c8b2a057cc2,0x38085a65b744237d
  ,0xcaf0303da21ecc16,0xc660be5561054445,0xeaf4360fc8aa9031,0xd60926fff096d2ab,0x1a03ac007c232799
  ,0xe584496fb0608fc0,0x09079c518d7206e3,0x4c5a70bebce35d84,0xf8200f7b4dae3d6a,0xb4a6c4c4b5d16a38
  ,0x5451bc4e7fb2cfeb,0x6525fd92075937b6,0x406eae19f78ec53e,0x6bf94e6694a523ec,0xfeb03e90e7faba04
  ,0xf10a0d43f2c28d4b,0xe44aa8f952a71ce4,0x6ef966e88c95e8cc,0x1f0a91de603823fe,0x540fc5e2212688c5
  ,0x98538a3191aa00c4,0x82a866243999920b,0x7c531773ea9a576e,0x683e1d213a11048e,0x001441c42aea5812
  ,0x278d5e7726567c30,0xe0af4e322898ba88,0x6693727353fd7cb0,0x87fc50f2604236e4,0x888c720e479ba9ba
  ,0x9aad9422fa1cd5ce,0x77b7d3c79b10764a,0x693f8172f63598a7,0x61daefcdebed2f6e,0x999306f3808e557e
  ,0x5d58c9725e45aed4,0x56aa23006a204e29,0x7b9f38d5f920cb7b,0xd1a79fbb76b795d0,0xe268fbe17c6672d3
  ,0xb8d8989b3d3dba70,0xa12c3645bb8cdf93,0x732cb315500e9ff6,0x45614a09f768ab04,0xd7a44a81f4a4626d
  ]

rndWords2 = 
  [0x888efd3a8e9d3d0f,0x3f7c8015929c4d0b,0x9dd1576dfee9144d,0x706e50d93edb988f,0x1bf0fbd22cb2fe42
  ,0xea3cb4e94f17df4a,0xf24bc3fb3eb11658,0xfdafd48ff4a6560e,0xf3fb721e97c24e28,0xe4189f3b29bab63c
  ,0x9888e3eed7a10abe,0x3f655e3ac3f99dfe,0xbdaf179b0ab70e1b,0x933489e815343e8d,0xc552296865200a2b
  ,0x0d7ee80f467eeb04,0xfd6b778e8babe925,0x1bb1255de3cd8786,0xc4a3b6f573ec6af1,0xd2dbf7d5b6a3be2e
  ,0xa1e22b70a36c96f8,0xa617f27f72fe8ff2,0x1e5024e9159ff0ae,0x603f5a9c50986495,0xe2f17131fb59bd93
  ,0x8d01a921a59646b7,0xc85ae16a975b9e97,0x3ff90c245b9ad063,0x4889782b52890c9f,0xa79b24111b5558d9
  ,0x9bc09895641cdd04,0xda8a2359ad7d0335,0x4e925260435f69a0,0x28fd2829993ee9b7,0xb38af0536c569fee
  ,0x78462beafef9e57c,0x2613166891ab270e,0x13775c45b6efb6c4,0x69921e49759bdda0,0x68bfc6f1643ff245
  ,0xf7dcc717f0549f2d,0x423e6b5bbfa2a8b6,0x5555fe6cc05d5519,0xc90f85f121c6adf0,0x68ec14dc41c1174d
  ,0x231473484795b255,0xa13c37dfbf1bfc44,0x5e3831b09836a34b,0x93fb41b63eedc8f8,0x07b3f59a37165884
  ,0x0b760cb359a1b6af,0x478b77fb0f54786e,0x024b1a3ddc880e54,0x3d565cd1272a3d81,0x9984a2d9ab5a4741
  ,0x8234b289e02f7aec,0x24e65cdf39319ded,0x7d3fd5f3b4f6b5a2,0x9289ff5cc281dcbd,0xb189bc7fee3c6c7c
  ,0x14f628aa402b4ab0,0x9dbe78e5f46e9e33,0xe2eeee94e9b874c3,0x6c236ec875b93341,0xf0429b07692c691b
  ]

{- 
-- generate random words
import Data.List ; import Control.Monad ; import System.Random ; import Data.Word ; import Text.Printf
main = do
  list <- replicateM 65 randomIO :: IO [Word64]
  putStrLn $ "[" ++ intercalate "," (map (printf "0x%016x") list) ++ "]"
-}

--------------------------------------------------------------------------------

w64_lists_of_length :: Int -> [[Word64]]
w64_lists_of_length ni =
  [ [ i | i<-[1..n]  ]
  , [ i | i<-[1001+n,1000+n .. 1001] ]
  , [ i | i<-[2^32-1-n .. 2^32-1   ] ]
  , [ i | i<-[2^32-1   .. 2^32+n-2 ] ]
  , [ i | i<-[2^64-1-n .. 2^64-1   ] ]
  , take ni rndWords1
  , take ni rndWords2
  , take ni $ reverse rndWords1
  , take ni $ reverse rndWords2
  ]
  where
    n = fromIntegral ni :: Word64

w64_lists = concatMap w64_lists_of_length [1..33]  :: [[Word64]]
blobs     = map blobFromWordList w64_lists            :: [Blob]

blob_pairs = [ (b1,b2)| b1<-blobs, b2<-blobs ] :: [(Blob,Blob)]

byte_lists = [ map fromIntegral [1..n] | n<-[1..(512::Int)] ] :: [[Word8]]

--------------------------------------------------------------------------------

eqBlob_naive b1 b2 = blobToWordList b1 == blobToWordList b2

baToByteList :: ByteArray -> [Word8]
baToByteList = foldrByteArray (:) [] 

pad :: [Word8] -> [Word8]
pad xs = xs ++ replicate (len8-len) 0 where
  len  = length xs
  len8 = 8 * (div (len+7) 8)

local_longZipWith :: Integral a => (a -> a -> b) -> [a] -> [a] -> [b]
local_longZipWith f (x:xs) (y:ys) = f x y : local_longZipWith f xs ys
local_longZipWith f xs [] = zipWith f xs (repeat 0)
local_longZipWith f [] ys = zipWith f (repeat 0) ys

eqWithZeros :: Blob -> Blob -> Bool
eqWithZeros a b = and $ local_longZipWith (==) (blobToWordList a) (blobToWordList b)

--------------------------------------------------------------------------------
-- * properties

prop_from_to_list list = blobToWordList (blobFromWordList list) == list
prop_to_from_blob blob = blobFromWordList (blobToWordList blob) == blob

prop_tolist_vs_peek blob = [ peekWord blob i | i<-[0..n-1] ] == blobToWordList blob where 
  n = blobSizeInWords blob

prop_head_of_list list = blobHead (blobFromWordList list) == L.head list

prop_eq_vs_naive (b1,b2) = eqBlob b1 b2 == eqBlob_naive b1 b2 

prop_to_from_bytearray blob = blobFromByteArray (blobToByteArray blob) == blob 

prop_from_to_bytearray list = baToByteList (blobToByteArray (blobFromByteArray ba)) == pad list where
  ba = byteArrayFromList list 

--------------------------------------------------------------------------------

prop_tail_cons         blob  =  blobTail (blobConsWord 0x1234567890abcdef blob) == blob
prop_shiftR64_shiftL64 blob  =  shiftR_by64 (shiftL_by64 blob) == blob
prop_shiftL64_shiftR64 blob  =  shiftL_by64 (shiftR_by64 blob) == blobConsWord 0 (blobTail blob)

prop_shiftR_shiftL blob = and [ shiftR (shiftL blob i) i `eqWithZeros` blob | i<-[0..193] ]

--------------------------------------------------------------------------------
