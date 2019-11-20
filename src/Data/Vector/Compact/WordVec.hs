
-- | Vector of (small) words which adapt their representation 
-- to make them more compact when the elements are small.
--
-- This is data structure engineered to store large amount of 
-- small vectors of small elements compactly on memory.
-- 
-- For example the list @[1..14] :: [Int]@ consumes 576 bytes (72 words) on 
-- a 64 bit machine, while the corresponding dynamic word vector takes only
-- 16 bytes (2 words), and the one corresponding to @[101..114]@ still only 
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
-- TODO: ability to add user-defined (fixed-length) header, it can be useful
-- for some applications
--

{-# LANGUAGE BangPatterns #-}
module Data.Vector.Compact.WordVec where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word

import Data.Vector.Compact.Blob 

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
-- * Empty vectors

empty :: WordVec
empty = fromList []

null :: WordVec -> Bool
null v = (vecLen v == 0)

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
    
head :: WordVec -> Word
head dynvec@(WordVec blob) = 
  case vecIsSmall dynvec of
    True  -> extractSmallWord bits blob  8
    False -> extractSmallWord bits blob 32
  where
    bits = vecBits dynvec

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
                
-- | Another implementation of 'toList', for testing purposes only
toList_naive :: WordVec -> [Word]
toList_naive dynvec@(WordVec blob)  = 
  case isSmall of
    True  -> [ extractSmallWord bits blob ( 8 + bits*i) | i<-[0..len-1] ]
    False -> [ extractSmallWord bits blob (32 + bits*i) | i<-[0..len-1] ]
  where
    (isSmall, Shape len bits) = vecShape' dynvec

--------------------------------------------------------------------------------
    
fromList :: [Word] -> WordVec
fromList [] = fromList' (Shape 0 4) []
fromList xs = fromList' (Shape l b) xs where
  l = length xs
  b = bitsNeededFor (maximum xs)
  
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
-- * Some more operations

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
naiveZipWith f u v = fromList $ zipWith f (toList u) (toList v)

-- | If you have a (nearly sharp) upper bound to the result of your of function
-- on your vector, zipping can be more efficient 
boundedZipWith :: Word -> (Word -> Word -> Word) -> WordVec -> WordVec -> WordVec
boundedZipWith bound f vec1 vec2  = fromList' (Shape l bits) $ zipWith f (toList vec1) (toList vec2) where
  l    = min (vecLen vec1) (vecLen vec2)
  bits = bitsNeededFor bound
              
--------------------------------------------------------------------------------
-- * Misc helpers

bitsNeededFor :: Word -> Int
bitsNeededFor bound = ceilingLog2 (bound + 1) where      -- for example, if maximum is 16, log2 = 4 but we need 5 bits

  -- | Smallest integer @k@ such that @2^k@ is larger or equal to @n@
  ceilingLog2 :: Word -> Int
  ceilingLog2 0 = 0
  ceilingLog2 n = 1 + go (n-1) where
    go 0 = -1
    go k = 1 + go (shiftR k 1)

--------------------------------------------------------------------------------

