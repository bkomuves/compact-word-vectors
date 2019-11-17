
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
-- Some operations may be a bit slower, but hopefully the cache-friendlyness will
-- somewhat balance that. TODO: measurements. In any case the primary goal here 
-- is optimized memory usage.
--

{-# LANGUAGE CPP, BangPatterns #-}
module Dynamic where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word

import Blob 

--------------------------------------------------------------------------------
 
fi :: Int -> Word
fi = fromIntegral

test1a !from !len = list == toList (fromList list) where list = map fi [from..from+len-1]
test1b !from !len = list == toList (fromList list) where list = reverse $ map fi [from..from+len-1]
 
test2a !from !len = fromList list == fromList (toList (fromList list)) where list = map fi [from..from+len-1]
test2b !from !len = fromList list == fromList (toList (fromList list)) where list = reverse $ map fi [from..from+len-1]

bad1a n m = [ (from,len) | from<-[0..n], len<-[0..m], not (test1a from len) ]    
bad1b n m = [ (from,len) | from<-[0..n], len<-[0..m], not (test1b from len) ]    

bad2a n m = [ (from,len) | from<-[0..n], len<-[0..m], not (test2a from len) ]    
bad2b n m = [ (from,len) | from<-[0..n], len<-[0..m], not (test2b from len) ]    

--------------------------------------------------------------------------------

-- | Dynamic word vectors are internally 'Blob'-s, which start their shape,
-- and after that their content.
--
-- * small vectors has 2 bits of \"resolution\" and  6 bits of length
-- * big   vectors has 3 bits of \"resolution\" and 29 bits of length
--
-- Resolution encodes the number of bits per element. The latter is always a multiple
-- of 4 (that is: 4 bits per element, or 8, or 12, etc. up to 64 bits per element).
--
data DynWordVec
  = SmallWordVec {-# UNPACK #-} !Blob   -- ^ length at most 63 and elements at most @2^16-1@
  | BigWordVec   {-# UNPACK #-} !Blob   -- ^ length at most @2^29-1@ and elements at most @2^32-1@
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
vecShape dynvec = 
  case dynvec of
    SmallWordVec blob -> let h = blobHead blob in mkShape (shiftR h 2 .&. 63        ) (shiftL ((h.&.3)+1) 2)
    BigWordVec   blob -> let h = blobHead blob in mkShape (shiftR h 3 .&. 0x1fffffff) (shiftL ((h.&.7)+1) 2)
  where
    mkShape :: Word64 -> Word64 -> Shape
    mkShape x y = Shape (fromIntegral x) (fromIntegral y)

vecLen, vecBits :: DynWordVec -> Int
vecLen  = shapeLen  . vecShape
vecBits = shapeBits . vecShape

--------------------------------------------------------------------------------

toList :: DynWordVec -> [Word]
toList dynvec =
  case dynvec of
    SmallWordVec blob -> worker  8 len (onHeadShiftR  8 $ blobToWordList blob)
    BigWordVec   blob -> worker 32 len (onHeadShiftR 32 $ blobToWordList blob)
  
  where
  
    onHeadShiftR :: Int -> [Word64] -> [Word64]
    onHeadShiftR k (x:xs) = shiftR x k : xs
    
    Shape len bits = vecShape dynvec
    
    the_mask = shiftL 1 bits - 1 :: Word64

    mask :: Word64 -> Word
    mask w   = fromIntegral (w .&. the_mask)

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

-- | Another implementation of 'toList', for testing purposes only
toList_naive :: DynWordVec -> [Word]
toList_naive dynvec = 
  case dynvec of
    SmallWordVec blob -> [ extractSmallWord bits blob ( 8 + bits*i) | i<-[0..len-1] ]
    BigWordVec   blob -> [ extractSmallWord bits blob (32 + bits*i) | i<-[0..len-1] ]
  where
    shape@(Shape len bits) = vecShape dynvec

--------------------------------------------------------------------------------
    
fromList :: [Word] -> DynWordVec
fromList [] = fromList' (Shape 0 4) []
fromList xs = fromList' (Shape l b) xs where
  l = length xs
  b = ceilingLog2 (maximum xs + 1)       -- for example, if maximum is 16, log2 = 4 but we need 5 bits

fromList' :: Shape -> [Word] -> DynWordVec
fromList' (Shape len bits0) words
  | bits <= 16 && len <= 63  = SmallWordVec $ mkBlob (mkHeader 2)  8 words
  | otherwise                = BigWordVec   $ mkBlob (mkHeader 3) 32 words
  
  where
    !bits    = max 4 $ min 32 $ (bits0 + 3) .&. 0x3c
    !bitsEnc = shiftR bits 2 - 1 :: Int
    !content = bits*len          :: Int
    !mask    = shiftL 1 bits - 1 :: Word64

    mkHeader :: Int -> Word64
    mkHeader !resoBits = fromIntegral (bitsEnc + shiftL len resoBits)
     
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
              
-- | Smallest integer @k@ such that @2^k@ is larger or equal to @n@
ceilingLog2 :: Word -> Int
ceilingLog2 0 = 0
ceilingLog2 n = 1 + go (n-1) where
  go 0 = -1
  go k = 1 + go (shiftR k 1)

--------------------------------------------------------------------------------

