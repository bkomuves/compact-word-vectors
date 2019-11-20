
-- | Signed integer version of dynamic word vectors.

{-# LANGUAGE BangPatterns #-}
module IntVec where

--------------------------------------------------------------------------------

import Data.Bits
import Data.List

import Dynamic ( Shape(..) )
import qualified Dynamic as Dyn

--------------------------------------------------------------------------------
-- * The dynamic int vector type

-- | A dynamic int vector is a (small) vector of small signed integers stored compactly
newtype IntVec 
  = IntVec Dyn.DynWordVec
  deriving (Eq,Ord)
    
vecShape :: IntVec -> Shape  
vecShape (IntVec dyn) = Dyn.vecShape dyn

vecLen :: IntVec -> Int
vecLen (IntVec dyn) = Dyn.vecLen dyn 

vecBits :: IntVec -> Int
vecBits (IntVec dyn) = Dyn.vecBits dyn

--------------------------------------------------------------------------------
  
instance Show IntVec where
  showsPrec = showsPrecIntVec

showIntVec :: IntVec -> String
showIntVec vec = showsPrecIntVec 0 vec []

showsPrecIntVec :: Int -> IntVec -> ShowS
showsPrecIntVec prec intvec
  = showParen (prec > 10) 
  $ showString "fromList "
--  . showsPrec 11 (lenMinMax intvec)
  . showChar ' ' 
  . shows (toList intvec)

--------------------------------------------------------------------------------
-- * Empty
  
empty :: IntVec
empty = fromList []

null :: IntVec -> Bool
null v = vecLen v == 0

--------------------------------------------------------------------------------
-- * Conversion from\/to lists

toList :: IntVec -> [Int]
toList (IntVec dynvec) = map (word2int bits) $ Dyn.toList dynvec where
  !bits = Dyn.vecBits dynvec

fromList :: [Int] -> IntVec
fromList xs = IntVec $ Dyn.fromList' (Dyn.Shape len bits) $ map (int2word bits) xs where
  (!len,!minMax) = lenMinMax xs
  !bits = roundBits (bitsNeededForMinMax minMax)

fromList' :: (Int,(Int,Int)) -> [Int] -> IntVec
fromList' (!len,!minMax) xs = IntVec $ Dyn.fromList' (Dyn.Shape len bits) $ map (int2word bits) xs where
  !bits = roundBits (bitsNeededForMinMax minMax)

lenMinMax :: [Int] -> (Int,(Int,Int))
lenMinMax = go 0 0 0 where
  go !cnt !p !q (x:xs) = go (cnt+1) (min x p) (max x q) xs
  go !cnt !p !q []     = (cnt,(p,q))

int2word :: Int -> (Int -> Word)
int2word !bits = i2w where
  !mask = shiftL 1 bits - 1 :: Word

  i2w :: Int -> Word
  i2w x = (fromIntegral x :: Word) .&. mask

word2int :: Int -> (Word -> Int)
word2int !bits = w2i where
  !mask = shiftL 1 bits - 1  :: Word
  !ffff = complement mask :: Word
  !bitsMinus1 = bits - 1

  w2i :: Word -> Int
  w2i x = case testBit x bitsMinus1 of
    False -> fromIntegral           x      -- non-negative
    True  -> fromIntegral (ffff .|. x)     -- negative

--------------------------------------------------------------------------------
-- * Indexing

unsafeIndex :: Int -> IntVec -> Int
unsafeIndex idx (IntVec dynvec) = word2int bits (Dyn.unsafeIndex idx dynvec) where
  !bits = Dyn.vecBits dynvec

safeIndex :: Int -> IntVec -> Maybe Int
safeIndex idx (IntVec dynvec) = (word2int bits) <$> (Dyn.safeIndex idx dynvec) where
  !bits = Dyn.vecBits dynvec
    
head :: IntVec -> Int
head (IntVec dynvec) = word2int bits (Dyn.head dynvec) where
  !bits = Dyn.vecBits dynvec

--------------------------------------------------------------------------------

{-
concat :: IntVec -> IntVec -> IntVec
concat u v = fromList' (Shape (lu+lv) (max bu bv)) (toList u ++ toList v) where
  Shape lu bu = vecShape u
  Shape lv bv = vecShape v
-}
  
--------------------------------------------------------------------------------
-- * helpers for counting necessary bits

bitsNeededForMinMax :: (Int,Int) -> Int
bitsNeededForMinMax (p,q) = max (bitsNeededFor p) (bitsNeededFor q)

bitsNeededFor :: Int -> Int
bitsNeededFor bound 
  | bound >= 0  = ceilingLog2 (    bound + 1) + 1   -- +8 needs 5 bits (-16..+15)
  | bound <  0  = ceilingLog2 (abs bound    ) + 1   -- -8 needs 4 bits (-8 ..+7 )
  where 

    -- | Smallest integer @k@ such that @2^k@ is larger or equal to @n@
    ceilingLog2 :: Int -> Int
    ceilingLog2 0 = 0
    ceilingLog2 n = 1 + go (n-1) where
      go 0 = -1
      go k = 1 + go (shiftR k 1)

-- | We only allow multiples of 4.
roundBits :: Int -> Int
roundBits 0 = 4
roundBits k = shiftL (shiftR (k+3) 2) 2

--------------------------------------------------------------------------------

