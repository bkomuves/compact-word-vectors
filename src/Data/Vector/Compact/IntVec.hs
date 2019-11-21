
-- | Signed integer version of dynamic word vectors.
--
-- See "Data.Vector.Compact.WordVec" for more details.
--
-- Note: for unsigned integers, you really should use 'WordVec' instead, 
-- because that is significantly faster.
--
-- This module should be imported qualified (to avoid name clashes with Prelude).
--

{-# LANGUAGE BangPatterns #-}
module Data.Vector.Compact.IntVec where

--------------------------------------------------------------------------------

import Prelude hiding ( head , tail , null ) 
import qualified Data.List as L

import Data.Bits

import Data.Vector.Compact.WordVec ( Shape(..) )
import qualified Data.Vector.Compact.WordVec as Dyn

--------------------------------------------------------------------------------
-- * The dynamic Int vector type

-- | A dynamic int vector is a (small) vector of small signed integers stored compactly
newtype IntVec 
  = IntVec Dyn.WordVec
  -- deriving (Eq,Ord)   -- WARNING: deriving Eq and Ord result in INCORRECT instances!!

instance Eq IntVec where 
  (==) x y  =  (vecLen x == vecLen y) && (toList x == toList y)

instance Ord IntVec where
  compare x y = case compare (vecLen x) (vecLen y) of 
    LT -> LT
    GT -> GT
    EQ -> compare (toList x) (toList y)
  
    
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
-- * Empty, singleton
  
empty :: IntVec
empty = fromList []

null :: IntVec -> Bool
null v = vecLen v == 0

singleton :: Int -> IntVec 
singleton i = fromList [i]

isSingleton :: IntVec -> Maybe Int
isSingleton (IntVec dynvec) = case Dyn.isSingleton dynvec of
  Nothing -> Nothing
  Just w  -> Just $ word2int (Dyn.vecBits dynvec) w

--------------------------------------------------------------------------------
-- * Conversion from\/to lists

toList :: IntVec -> [Int]
toList (IntVec dynvec) = map (word2int bits) $ Dyn.toList dynvec where
  !bits = Dyn.vecBits dynvec

toRevList :: IntVec -> [Int]
toRevList (IntVec dynvec) = map (word2int bits) $ Dyn.toRevList dynvec where
  !bits = Dyn.vecBits dynvec

fromList :: [Int] -> IntVec
fromList xs = IntVec $ Dyn.fromList' (Dyn.Shape len bits) $ map (int2word bits) xs where
  (!len,!minMax) = lenMinMax xs
  !bits = roundBits (bitsNeededForMinMax minMax)

fromList' :: (Int,(Int,Int)) -> [Int] -> IntVec
fromList' (!len,!minMax) xs = IntVec $ Dyn.fromList' (Dyn.Shape len bits) $ map (int2word bits) xs where
  !bits = roundBits (bitsNeededForMinMax minMax)

-- | Don't use this unless you really know what you are doing!
fromList'' :: Shape -> [Int] -> IntVec
fromList'' shape@(Shape len !bits) xs = IntVec $ Dyn.fromList' shape $ map (int2word bits) xs 

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- * Head, tail, etc
    
head :: IntVec -> Int
head (IntVec dynvec) = word2int bits (Dyn.head dynvec) where
  !bits = Dyn.vecBits dynvec

last :: IntVec -> Int
last (IntVec dynvec) = word2int bits (Dyn.last dynvec) where
  !bits = Dyn.vecBits dynvec

tail :: IntVec -> IntVec
tail (IntVec dynvec) = IntVec (Dyn.tail dynvec)

uncons :: IntVec -> Maybe (Int,IntVec)
uncons (IntVec dynvec) = case Dyn.uncons dynvec of
  Nothing     -> Nothing
  Just (w,tl) -> Just (word2int bits w , IntVec tl)
  where
    bits = Dyn.vecBits dynvec

-- | For testing purposes only
uncons_naive :: IntVec -> Maybe (Int,IntVec)
uncons_naive vec = if null vec 
  then Nothing
  else Just (head vec, tail vec)

cons :: Int -> IntVec -> IntVec
cons k ivec@(IntVec vec) = IntVec $ Dyn.fromList' shape' $ map (int2word bits') (k : toList ivec) where
  (Shape len bits) = Dyn.vecShape vec
  bits'  = roundBits $ max bits (bitsNeededFor k)
  shape' = Shape (len+1) bits'

--------------------------------------------------------------------------------

{-
concat :: IntVec -> IntVec -> IntVec
concat u v = fromList' (Shape (lu+lv) (max bu bv)) (toList u ++ toList v) where
  Shape lu bu = vecShape u
  Shape lv bv = vecShape v
-}

--------------------------------------------------------------------------------
-- * Some more operations

naiveMap :: (Int -> Int) -> IntVec -> IntVec
naiveMap f u = fromList (map f $ toList u)

-- | If you have (nearly sharp) lower and upper bounds for the result of your of function
-- on your vector, mapping can be more efficient 
boundedMap :: (Int,Int) -> (Int -> Int) -> IntVec -> IntVec
boundedMap minMax f vec = fromList'' (Shape l bits) (toList vec) where
  l    = vecLen vec
  bits = roundBits $ bitsNeededForMinMax minMax

concat :: IntVec -> IntVec -> IntVec
concat u v = fromList'' (Shape (lu+lv) (max bu bv)) (toList u ++ toList v) where
  Shape lu bu = vecShape u
  Shape lv bv = vecShape v

naiveZipWith :: (Int -> Int -> Int) -> IntVec -> IntVec -> IntVec
naiveZipWith f u v = fromList $ L.zipWith f (toList u) (toList v)

-- | If you have (nearly sharp) lower and upper bounds for the result of your of function
-- on your vector, zipping can be more efficient 
boundedZipWith :: (Int,Int) -> (Int -> Int -> Int) -> IntVec -> IntVec -> IntVec
boundedZipWith minMax f vec1 vec2  = fromList'' (Shape l bits) $ L.zipWith f (toList vec1) (toList vec2) where
  l    = min (vecLen vec1) (vecLen vec2)
  bits = roundBits $ bitsNeededForMinMax minMax

listZipWith :: (Int -> Int -> a) -> IntVec -> IntVec -> [a]
listZipWith f u v = L.zipWith f (toList u) (toList v)
  
--------------------------------------------------------------------------------
-- * helpers for counting the necessary number of bits

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

