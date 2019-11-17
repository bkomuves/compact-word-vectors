
-- | Vector of (small) words which adapt their representation 
-- to make them more compact when the elements are small.
--
-- This is data structure engineered to store large amount of 
-- small vectors of small elements compactly on memory.
-- 
-- For example the list @[1..14] :: [Int]@ consumes 576 bytes (72 words) on 
-- a 64 bit machine, while the corresponding dynamic vector takes only
-- 16 bytes (2 words), and the list @[101..114]@ still only 24 bytes (3 words).
--
-- Some operations may be a bit slower, but hopefully the cache-friendlyness 
-- will somewhat balance that (a simple microbenchmark with 'Data.Map'-s
-- indexed by @[Int]@ vs. @DynWordVec@ showed a 2x improvement in speed and
-- 20x improvement in memory usage). In any case the primary goal
-- here is optimized memory usage.
--
-- TODO: ability to add user-defined (fixed-length) header, it can be useful
-- for some applications
--

{-# LANGUAGE CPP, BangPatterns #-}
module Dynamic where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word

import Blob 

--------------------------------------------------------------------------------
-- * The dynamic word vector type

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
newtype DynWordVec = DynWordVec Blob
  -- deriving Show

instance Show DynWordVec where
  showsPrec = showsPrecDynWordVec

showDynWordVec :: DynWordVec -> String
showDynWordVec dynvec = showsPrecDynWordVec 0 dynvec []

showsPrecDynWordVec :: Int -> DynWordVec -> ShowS
showsPrecDynWordVec prec dynvec
    = showParen (prec > 10) 
    $ showString "fromList' "
    . showsPrec 11 (vecShape dynvec)
    . showChar ' ' 
    . shows (toList dynvec)
    
instance Eq DynWordVec where
  (==) x y  =  (vecLen x == vecLen y) && (toList x == toList y)

instance Ord DynWordVec where
  compare x y = case compare (vecLen x) (vecLen y) of 
    LT -> LT
    GT -> GT
    EQ -> compare (toList x) (toList y)
    
data Shape = Shape
  { shapeLen  :: !Int      -- ^ length of the vector
  , shapeBits :: !Int      -- ^ bits per element (quantized to multiples of 4)
  }
  deriving (Eq,Show)

vecShape :: DynWordVec -> Shape
vecShape = snd . vecShape'
  
vecShape' :: DynWordVec -> (Bool,Shape)
vecShape' (DynWordVec blob) = (isSmall,shape) where
  !h      = blobHead blob
  !h2     = shiftR h 1
  !isSmall = (h .&. 1) == 0
  shape   = if isSmall
    then mkShape (shiftR h 3 .&. 31        ) (shiftL ((h2.&. 3)+1) 2)
    else mkShape (shiftR h 5 .&. 0x07ffffff) (shiftL ((h2.&.15)+1) 2)
  mkShape :: Word64 -> Word64 -> Shape
  mkShape !x !y = Shape (fromIntegral x) (fromIntegral y)

vecIsSmall :: DynWordVec -> Bool
vecIsSmall (DynWordVec blob) = (blobHead blob .&. 1) == 0  

vecLen, vecBits :: DynWordVec -> Int
vecLen  = shapeLen  . vecShape
vecBits = shapeBits . vecShape

null :: DynWordVec -> Bool
null v = (vecLen v == 0)

--------------------------------------------------------------------------------
-- * Indexing

unsafeIndex :: Int -> DynWordVec -> Word
unsafeIndex idx dynvec@(DynWordVec blob) = 
  case isSmall of
    True  -> extractSmallWord bits blob ( 8 + bits*idx)
    False -> extractSmallWord bits blob (32 + bits*idx)
  where
    (isSmall, Shape _ bits) = vecShape' dynvec

safeIndex :: Int -> DynWordVec -> Maybe Word
safeIndex idx dynvec@(DynWordVec blob)
  | idx < 0    = Nothing
  | idx >= len = Nothing
  | otherwise  = Just $ case isSmall of
      True  -> extractSmallWord bits blob ( 8 + bits*idx)
      False -> extractSmallWord bits blob (32 + bits*idx)
  where
    (isSmall, Shape len bits) = vecShape' dynvec
    
--------------------------------------------------------------------------------
-- * Conversion to\/from lists

toList :: DynWordVec -> [Word]
toList dynvec@(DynWordVec blob) =
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
                [] -> error "DynWordVec/toList: FATAL ERROR! this should not happen"
                
-- | Another implementation of 'toList', for testing purposes only
toList_naive :: DynWordVec -> [Word]
toList_naive dynvec@(DynWordVec blob)  = 
  case isSmall of
    True  -> [ extractSmallWord bits blob ( 8 + bits*i) | i<-[0..len-1] ]
    False -> [ extractSmallWord bits blob (32 + bits*i) | i<-[0..len-1] ]
  where
    (isSmall, Shape len bits) = vecShape' dynvec

--------------------------------------------------------------------------------
    
fromList :: [Word] -> DynWordVec
fromList [] = fromList' (Shape 0 4) []
fromList xs = fromList' (Shape l b) xs where
  l = length xs
  b = bitsNeededFor (maximum xs)
  
fromList' :: Shape -> [Word] -> DynWordVec
fromList' (Shape len bits0) words
  | bits <= 16 && len <= 31  = DynWordVec $ mkBlob (mkHeader 0 2)  8 words
  | otherwise                = DynWordVec $ mkBlob (mkHeader 1 4) 32 words
  
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
-- * some operations

naiveMap :: (Word -> Word) -> DynWordVec -> DynWordVec
naiveMap f u = fromList (map f $ toList u)

-- | If you have a (nearly sharp) upper bound to the result of your of function
-- on your vector, mapping can be more efficient 
boundedMap :: Word -> (Word -> Word) -> DynWordVec -> DynWordVec
boundedMap bound f vec = fromList' (Shape l bits) (toList vec) where
  l    = vecLen vec
  bits = bitsNeededFor bound

concat :: DynWordVec -> DynWordVec -> DynWordVec
concat u v = fromList' (Shape (lu+lv) (max bu bv)) (toList u ++ toList v) where
  Shape lu bu = vecShape u
  Shape lv bv = vecShape v

naiveZipWith :: (Word -> Word -> Word) -> DynWordVec -> DynWordVec -> DynWordVec
naiveZipWith f u v = fromList $ zipWith f (toList u) (toList v)

-- | If you have a (nearly sharp) upper bound to the result of your of function
-- on your vector, zipping can be more efficient 
boundedZipWith :: Word -> (Word -> Word -> Word) -> DynWordVec -> DynWordVec -> DynWordVec
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

