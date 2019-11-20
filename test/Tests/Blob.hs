
-- | Tests for blobs

{-# LANGUAGE CPP,BangPatterns #-}
module Tests.Blob where

--------------------------------------------------------------------------------

import Data.Word
import Data.List as L

import Data.Primitive.ByteArray

import Data.Vector.Compact.Blob

import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------

all_tests = tests_blobs

tests_blobs = testGroup "unit tests for Blobs"
  [ testCase "toList . fromList == id"    $ forall_ w64_lists   prop_from_to_list
  , testCase "fromList . toList == id"    $ forall_ blobs       prop_to_from_blob
  , testCase "toList vs. peekWord"        $ forall_ blobs       prop_tolist_vs_peek
  , testCase "blobHead vs. list head"     $ forall_ w64_lists   prop_head_of_list
  , testCase "eqBlob vs. naive"           $ forall_ blob_pairs  prop_eq_vs_naive
  , testCase "fromBA . toBA == id"        $ forall_ blobs       prop_to_from_bytearray
  , testCase "toBA . fromBA == pad"       $ forall_ byte_lists  prop_from_to_bytearray
  ]

forall_ :: [a] -> (a -> Bool) -> Assertion
forall_ xs cond = assertBool "failed" (and (map cond xs))

--------------------------------------------------------------------------------
-- * inputs

w64_lists_of_length :: Int -> [[Word64]]
w64_lists_of_length ni =
  [ [ i | i<-[1..n]  ]
  , [ i | i<-[1001+n,1000+n .. 1001] ]
  , [ i | i<-[2^32-1-n .. 2^32-1   ] ]
  , [ i | i<-[2^32-1   .. 2^32+n-2 ] ]
  , [ i | i<-[2^64-1-n .. 2^64-1   ] ]
  ]
  where
    n = fromIntegral ni :: Word64

w64_lists = concatMap w64_lists_of_length [1..33]  :: [[Word64]]
blobs     = map blobFromWordList w64_lists            :: [Blob]

blob_pairs = [ (b1,b2)| b1<-blobs, b2<-blobs ] :: [(Blob,Blob)]

byte_lists = [ map fromIntegral [1..n] | n<-[1..(512::Int)] ] :: [[Word8]]

--------------------------------------------------------------------------------

eqBlob_naive b1 b2 = blobToWordList b1 == blobToWordList b2

baToList :: ByteArray -> [Word8]
baToList = foldrByteArray (:) [] 

pad :: [Word8] -> [Word8]
pad xs = xs ++ replicate (len8-len) 0 where
  len  = length xs
  len8 = 8 * (div (len+7) 8)

--------------------------------------------------------------------------------
-- * properties

prop_from_to_list list = blobToWordList (blobFromWordList list) == list
prop_to_from_blob blob = blobFromWordList (blobToWordList blob) == blob

prop_tolist_vs_peek blob = [ peekWord blob i | i<-[0..n-1] ] == blobToWordList blob where 
  n = blobSizeInWords blob

prop_head_of_list list = blobHead (blobFromWordList list) == L.head list

prop_eq_vs_naive (b1,b2) = eqBlob b1 b2 == eqBlob_naive b1 b2 

prop_to_from_bytearray blob = blobFromByteArray (blobToByteArray blob) == blob 

prop_from_to_bytearray list = baToList (blobToByteArray (blobFromByteArray ba)) == pad list where
  ba = byteArrayFromList list 

--------------------------------------------------------------------------------
