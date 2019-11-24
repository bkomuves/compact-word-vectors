
-- | Vector of (small) words which adapt their representation 
-- to make them more compact when the elements are small.
--
-- This is data structure engineered to store large amount of 
-- small vectors of small elements compactly on memory.
-- 
-- For example the list @[1..14] :: [Int]@ consumes 560 bytes (14x5=70 words) on 
-- a 64 bit machine, while the corresponding 'WordVec' takes only
-- 16 bytes (2 words), and the one corresponding to @[101..115]@ still only 
-- 24 bytes (3 words).
--
-- Unboxed arrays or unboxed vectors are better, as they only have a constant
-- overhead, but those constants are big: 13 words (104 bytes on 64 bit)
-- for unboxed arrays, and 6 words (48 bytes) for unboxed vectors. And you
-- still have to select the number of bits per element in advance.
--
-- Some operations may be a bit slower, but hopefully the cache-friendlyness 
-- will somewhat balance that (a simple microbenchmark with 'Data.Map'-s
-- indexed by @[Int]@ vs. @WordVec@ showed a 2x improvement in speed and
-- 20x improvement in memory usage). In any case the primary goal
-- here is optimized memory usage.
--
-- This module should be imported qualified (to avoid name clashes with Prelude).
--
-- TODO: ability to add user-defined (fixed-length) header, it can be 
-- potentially useful for some applications 
--

{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
module Data.Vector.Compact.WordVec where

--------------------------------------------------------------------------------

import Prelude hiding ( head , tail , null ) 
import qualified Data.List as L

import Data.Bits
import Data.Word

import Foreign.C

import Data.Vector.Compact.Blob 

--------------------------------------------------------------------------------

-- ???? how to determine this properly... 
-- why on earth isn't this stuff properly documented?!?!?!?!? 
#ifdef x86_64_HOST_ARCH
#define MACHINE_WORD_BITS 64 
#elif i386_HOST_ARCH
#define MACHINE_WORD_BITS 32
#elif i686_HOST_ARCH
#define MACHINE_WORD_BITS 32
#elif aarch64_HOST_ARCH
#define MACHINE_WORD_BITS 64 
#else
#define MACHINE_WORD_BITS 32
#endif

--------------------------------------------------------------------------------
-- * The dynamic Word vector type

-- | Dynamic word vectors are internally 'Blob'-s, which the first few bits
-- encoding their shape, and after that their content.
--
-- * small vectors has 2 bits of \"resolution\" and  5 bits of length
-- * big   vectors has 4 bits of \"resolution\" and 27 bits of length
--
-- Resolution encodes the number of bits per element. The latter is always a multiple
-- of 4 (that is: 4 bits per element, or 8, or 12, etc. up to 64 bits per element).
--
-- We use the very first bit to decide which of these two encoding we use.
-- (if we would make a sum type instead, it would take 2 extra words...)
--
newtype WordVec = WordVec Blob
  -- deriving Show

-- | The \"shape\" of a dynamic word vector
data Shape = Shape
  { shapeLen  :: !Int      -- ^ length of the vector
  , shapeBits :: !Int      -- ^ bits per element (quantized to multiples of 4)
  }
  deriving (Eq,Show)

vecShape :: WordVec -> Shape
vecShape = snd . vecShape'
  
vecShape' :: WordVec -> (Bool,Shape)
vecShape' (WordVec blob) = (isSmall,shape) where
  !h      = blobHead blob
  !h2     = shiftR h 1
  !isSmall = (h .&. 1) == 0
  shape   = if isSmall
    then mkShape (shiftR h 3 .&. 31        ) (shiftL ((h2.&. 3)+1) 2)
    else mkShape (shiftR h 5 .&. 0x07ffffff) (shiftL ((h2.&.15)+1) 2)
  mkShape :: Word64 -> Word64 -> Shape
  mkShape !x !y = Shape (fromIntegral x) (fromIntegral y)

vecIsSmall :: WordVec -> Bool
vecIsSmall (WordVec blob) = (blobHead blob .&. 1) == 0  

vecLen, vecBits :: WordVec -> Int
vecLen  = shapeLen  . vecShape
vecBits = shapeBits . vecShape

--------------------------------------------------------------------------------
-- * Instances

instance Show WordVec where
  showsPrec = showsPrecWordVec

showWordVec :: WordVec -> String
showWordVec dynvec = showsPrecWordVec 0 dynvec []

showsPrecWordVec :: Int -> WordVec -> ShowS
showsPrecWordVec prec dynvec
  = showParen (prec > 10) 
  $ showString "fromList' "
  . showsPrec 11 (vecShape dynvec)
  . showChar ' ' 
  . shows (toList dynvec)
    
instance Eq WordVec where
  (==) x y  =  (vecLen x == vecLen y) && (toList x == toList y)

instance Ord WordVec where
  compare x y = case compare (vecLen x) (vecLen y) of 
    LT -> LT
    GT -> GT
    EQ -> compare (toList x) (toList y)

--------------------------------------------------------------------------------
-- * Empty vector, singleton

empty :: WordVec
empty = fromList []

null :: WordVec -> Bool
null (WordVec blob) = 
  -- null v = (vecLen v == 0)
  let !h = blobHead blob 
  in  (h .&. 0xf9 == 0) || (h .&. 0xffffffe1 == 1)
  -- 0xf9       = 000 ... 00|11111001
  -- 0xffffffe1 = 111 ... 11|11100001
   
null_naive :: WordVec -> Bool
null_naive v = (vecLen v == 0)

singleton :: Word -> WordVec
singleton x = fromListN 1 x [x] where

isSingleton :: WordVec -> Maybe Word
isSingleton v = case (vecLen v) of
  1 -> Just (head v)
  _ -> Nothing

--------------------------------------------------------------------------------
-- * Indexing

unsafeIndex :: Int -> WordVec -> Word
unsafeIndex idx dynvec@(WordVec blob) = 
  case isSmall of
    True  -> extractSmallWord bits blob ( 8 + bits*idx)
    False -> extractSmallWord bits blob (32 + bits*idx)
  where
    (isSmall, Shape _ bits) = vecShape' dynvec

safeIndex :: Int -> WordVec -> Maybe Word
safeIndex idx dynvec@(WordVec blob)
  | idx < 0    = Nothing
  | idx >= len = Nothing
  | otherwise  = Just $ case isSmall of
      True  -> extractSmallWord bits blob ( 8 + bits*idx)
      False -> extractSmallWord bits blob (32 + bits*idx)
  where
    (isSmall, Shape len bits) = vecShape' dynvec

--------------------------------------------------------------------------------
-- * Head, tail, etc
    
head :: WordVec -> Word
head dynvec@(WordVec blob) = 
  case vecIsSmall dynvec of
    True  -> extractSmallWord bits blob  8
    False -> extractSmallWord bits blob 32
  where
    bits = vecBits dynvec

last :: WordVec -> Word
last dynvec@(WordVec blob) 
  | len == 0   = 0
  | otherwise  = case isSmall of
    True  -> extractSmallWord bits blob ( 8 + bits*(len-1))
    False -> extractSmallWord bits blob (32 + bits*(len-1))
  where
    (isSmall, Shape len bits) = vecShape' dynvec

--------------------------------------------------------------------------------

tail   = tail_v2
cons   = cons_v2
uncons = uncons_v2

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

--------------------------------------------------------------------------------

foreign import ccall unsafe "vec_identity"  c_vec_identity  :: CFun11_       -- for testing
foreign import ccall unsafe "vec_tail"      c_vec_tail      :: CFun11_
foreign import ccall unsafe "vec_head_tail" c_vec_head_tail :: CFun11 Word64
foreign import ccall unsafe "vec_cons"      c_vec_cons      :: Word64 -> CFun11_

tail_v2 :: WordVec -> WordVec
tail_v2 (WordVec blob) = WordVec $ wrapCFun11_ c_vec_tail id blob

cons_v2 :: Word -> WordVec -> WordVec
cons_v2 y vec@(WordVec blob) = WordVec $ wrapCFun11_ (c_vec_cons (fromIntegral y)) f blob where
  f !n = max (n+1) worstcase
  len  = vecLen vec
  worstcase = shiftR (32 + bitsNeededFor y * (len+1) + 63) 6
  -- it can happen that we cons (2^64-1) to a long vector of 4 bit numbers...

uncons_v2 :: WordVec -> Maybe (Word,WordVec)
uncons_v2 vec@(WordVec blob) = if null vec 
  then Nothing
  else let (hd,tl) = wrapCFun11 c_vec_head_tail id blob
       in  Just (fromIntegral hd , WordVec tl)
       
--------------------------------------------------------------------------------
-- * Conversion to\/from lists

toList :: WordVec -> [Word]
toList dynvec@(WordVec blob) =
  case isSmall of
    True  -> worker  8 len (shiftR header  8 : restOfWords)
    False -> worker 32 len (shiftR header 32 : restOfWords)
  
  where
    isSmall = (header .&. 1) == 0
    (header:restOfWords) = blobToWordList blob
      
    Shape len bits = vecShape dynvec
    
    the_mask = shiftL 1 bits - 1 :: Word64

    mask :: Word64 -> Word
    mask w = fromIntegral (w .&. the_mask)

    worker !bitOfs !0 _  = []
    worker !bitOfs !k [] = replicate k 0     -- this shouldn't happen btw 
    worker !bitOfs !k (this:rest) = 
      let newOfs = bitOfs + bits 
      in  case compare newOfs 64 of
        LT -> (mask this) : worker newOfs (k-1) (shiftR this bits : rest)
        EQ -> (mask this) : worker 0      (k-1)                     rest
        GT -> case rest of 
                (that:rest') -> 
                  let !newOfs' = newOfs - 64
                      !elem = mask (this .|. shiftL that (64-bitOfs)) 
                  in  elem : worker newOfs' (k-1) (shiftR that newOfs' : rest') 
                [] -> error "WordVec/toList: FATAL ERROR! this should not happen"

-- | @toRevList vec == reverse (toList vec)@, but should be faster (?)
toRevList :: WordVec -> [Word] 
toRevList dynvec@(WordVec blob)  = 
  case isSmall of
    True  -> [ extractSmallWord bits blob ( 8 + bits*(len-i)) | i<-[1..len] ]
    False -> [ extractSmallWord bits blob (32 + bits*(len-i)) | i<-[1..len] ]
  where
    (isSmall, Shape len bits) = vecShape' dynvec

--------------------------------------------------------------------------------
    
fromList :: [Word] -> WordVec
fromList [] = fromList' (Shape 0 4) []
fromList xs = fromList' (Shape l b) xs where
  l = length xs
  b = bitsNeededFor (L.maximum xs)

-- | This is faster than 'fromList'
fromListN
 :: Int       -- ^ length
 -> Word      -- ^ maximum (or just an upper bound)
 -> [Word]    -- ^ elements
 -> WordVec
fromListN len max = fromList' (Shape len (bitsNeededFor max))
 
-- | If you know the shape in advance, it\'s faster to use this function 
fromList' :: Shape -> [Word] -> WordVec
fromList' (Shape len bits0) words
  | bits <= 16 && len <= 31  = WordVec $ mkBlob (mkHeader 0 2)  8 words
  | otherwise                = WordVec $ mkBlob (mkHeader 1 4) 32 words
  
  where
    !bits    = max 4 $ min 64 $ (bits0 + 3) .&. 0xfc
    !bitsEnc = shiftR bits 2 - 1 :: Int
    !content = bits*len          :: Int
    !mask    = shiftL 1 bits - 1 :: Word64

    mkHeader :: Word64 -> Int -> Word64
    mkHeader !isSmall !resoBits = isSmall + fromIntegral (shiftL (bitsEnc + shiftL len resoBits) 1)
     
    mkBlob !header !ofs words = blobFromWordListN (shiftR (ofs+content+63) 6) 
                              $ worker len header ofs words
    
    worker :: Int -> Word64 -> Int -> [Word] -> [Word64]
    worker  0 !current !bitOfs _           = if bitOfs == 0 then [] else [current] 
    worker !k !current !bitOfs []          = worker k current bitOfs [0]   
    worker !k !current !bitOfs (this0:rest) = 
      let !this     = (fromIntegral this0) .&. mask
          !newOfs   = bitOfs + bits 
          !current' = (shiftL this bitOfs) .|. current 
      in  case compare newOfs 64 of
        LT ->            worker (k-1) current' newOfs rest
        EQ -> current' : worker (k-1) 0        0      rest 
        GT -> let !newOfs' = newOfs - 64
              in   current' : worker (k-1) (shiftR this (64-bitOfs)) newOfs' rest

--------------------------------------------------------------------------------
-- * Specialized folds 
--
-- $folds
--
-- These are are faster than the generic operations below, and should be preferred.
--

-- | Sum of the elements of the vector
sum :: WordVec -> Word
sum (WordVec blob) = fromIntegral $ wrapCFun10 c_vec_sum blob

-- | Maximum of the elements of the vector
maximum :: WordVec -> Word
maximum (WordVec blob) = fromIntegral $ wrapCFun10 c_vec_max blob

foreign import ccall unsafe "vec_sum" c_vec_sum :: CFun10 Word64
foreign import ccall unsafe "vec_max" c_vec_max :: CFun10 Word64

--------------------------------------------------------------------------------
-- * Specialized zipping folds 
--
-- $zipfolds
--
-- These are are faster than the generic operations below, and should be preferred.
--

foreign import ccall unsafe "vec_equal_strict"  c_equal_strict  :: CFun20 CInt
foreign import ccall unsafe "vec_equal_extzero" c_equal_extzero :: CFun20 CInt
foreign import ccall unsafe "vec_less_or_equal" c_less_or_equal :: CFun20 CInt
foreign import ccall unsafe "vec_partial_sums_less_or_equal" c_partial_sums_less_or_equal :: CFun20 CInt

-- | Strict equality of vectors (same length, same content)
eqStrict :: WordVec -> WordVec -> Bool
eqStrict (WordVec blob1) (WordVec blob2) = (0 /= wrapCFun20 c_equal_strict blob1 blob2)

-- | Equality of vectors extended with zeros to infinity
eqExtZero :: WordVec -> WordVec -> Bool
eqExtZero (WordVec blob1) (WordVec blob2) = (0 /= wrapCFun20 c_equal_extzero blob1 blob2)

-- | Pointwise comparison of vectors extended with zeros to infinity
lessOrEqual :: WordVec -> WordVec -> Bool
lessOrEqual (WordVec blob1) (WordVec blob2) = (0 /= wrapCFun20 c_less_or_equal blob1 blob2)

-- | Pointwise comparison of partial sums of vectors extended with zeros to infinity
-- 
-- For example @[x1,x2,x3] <= [y1,y2,y3]@ iff (@x1 <=y1 && x1+x2 <= y1+y2 && x1+x2+x3 <= y1+y2+y3@).
--
partialSumsLessOrEqual :: WordVec -> WordVec -> Bool
partialSumsLessOrEqual (WordVec blob1) (WordVec blob2) =
  (0 /= wrapCFun20 c_partial_sums_less_or_equal blob1 blob2)

--------------------------------------------------------------------------------
-- * Specialized zips
--
-- $zips
--
-- These are are faster than the generic operations below, and should be preferred.
--

foreign import ccall unsafe "vec_add"  c_vec_add  :: CFun21_

-- | Pointwise addition of vectors. The shorter one is extended by zeros.
add :: WordVec -> WordVec -> WordVec
add vec1@(WordVec blob1) vec2@(WordVec blob2) = WordVec $ wrapCFun21_ c_vec_add f blob1 blob2 where
  -- WARNING! memory allocation is _very_ tricky here!
  -- worst case: we have a very long vector with 4 bits/elem,
  -- and a very short vector with 64 bits/elem!
  -- even @max b1 b2@ is not enough, because it can overflow...
  f _ _ = 1 + shiftR ( (max b1 b2 + 4)*(max l1 l2) + 63 ) 6
  Shape !l1 !b1 = vecShape vec1
  Shape !l2 !b2 = vecShape vec2

--------------------------------------------------------------------------------
-- * Some generic operations

fold :: (a -> Word -> a) -> a -> WordVec -> a
fold f x v = L.foldl' f x (toList v)  

naiveMap :: (Word -> Word) -> WordVec -> WordVec
naiveMap f u = fromList (map f $ toList u)

-- | If you have a (nearly sharp) upper bound to the result of your of function
-- on your vector, mapping can be more efficient 
boundedMap :: Word -> (Word -> Word) -> WordVec -> WordVec
boundedMap bound f vec = fromList' (Shape l bits) (toList vec) where
  l    = vecLen vec
  bits = bitsNeededFor bound

concat :: WordVec -> WordVec -> WordVec
concat u v = fromList' (Shape (lu+lv) (max bu bv)) (toList u ++ toList v) where
  Shape lu bu = vecShape u
  Shape lv bv = vecShape v

naiveZipWith :: (Word -> Word -> Word) -> WordVec -> WordVec -> WordVec
naiveZipWith f u v = fromList $ L.zipWith f (toList u) (toList v)

-- | If you have a (nearly sharp) upper bound to the result of your of function
-- on your vector, zipping can be more efficient 
boundedZipWith :: Word -> (Word -> Word -> Word) -> WordVec -> WordVec -> WordVec
boundedZipWith bound f vec1 vec2  = fromList' (Shape l bits) $ L.zipWith f (toList vec1) (toList vec2) where
  l    = min (vecLen vec1) (vecLen vec2)
  bits = bitsNeededFor bound

listZipWith :: (Word -> Word -> a) -> WordVec -> WordVec -> [a]
listZipWith f u v = L.zipWith f (toList u) (toList v)
              
--------------------------------------------------------------------------------
-- * Misc helpers

foreign import ccall unsafe "export_required_bits_not_rounded" export_required_bits_not_rounded :: Word64 -> CInt
foreign import ccall unsafe "export_required_bits"             export_required_bits             :: Word64 -> CInt

{-
-- apparently, the C implementation is _not_ faster...
bitsNeededFor :: Word -> Int
bitsNeededFor = fromIntegral . export_required_bits . fromIntegral

bitsNeededFor' :: Word -> Int
bitsNeededFor' = fromIntegral . export_required_bits_not_rounded . fromIntegral
-}

bitsNeededFor, bitsNeededFor' :: Word -> Int
bitsNeededFor  = bitsNeededForReference
bitsNeededFor' = bitsNeededForReference'

bitsNeededForReference :: Word -> Int
bitsNeededForReference = roundBits . bitsNeededForReference'

bitsNeededForReference' :: Word -> Int
bitsNeededForReference' bound 
  | bound   == 0  = 1                             -- this is handled incorrectly by the formula below
  | bound+1 == 0  = MACHINE_WORD_BITS             -- and this handled incorrectly because of overflow
  | otherwise     = ceilingLog2 (bound + 1)       -- for example, if maximum is 16, log2 = 4 but we need 5 bits 
  where    
    -- | Smallest integer @k@ such that @2^k@ is larger or equal to @n@
    ceilingLog2 :: Word -> Int
    ceilingLog2 0 = 0
    ceilingLog2 n = 1 + go (n-1) where
      go 0 = -1
      go k = 1 + go (shiftR k 1)

-- | We only allow multiples of 4.
roundBits :: Int -> Int
roundBits 0 = 4
roundBits k = shiftL (shiftR (k+3) 2) 2

--------------------------------------------------------------------------------

