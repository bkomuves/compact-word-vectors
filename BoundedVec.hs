
-- | Bounded word vectors are (small) vectors of small 'Word'-s which also store the
-- maximum value appearing in the vector. This may make some operations potentially
-- faster, and can be also useful for some applications.
--

module BoundedVec where

--------------------------------------------------------------------------------

import Data.List

import qualified Dynamic as Dyn

--------------------------------------------------------------------------------
-- * The bounded word vector type

-- | A bounded word vector is secretly just a dynamic word vector, whose first
-- element is used to store the maximum value.
newtype BoundedVec 
  = BoundedVec Dyn.DynWordVec
  deriving (Eq,Ord)
    
data BndShape = BndShape
  { bndLen :: !Int  
  , bndMax :: !Word
  }
  deriving (Eq,Show)
  
bndShape :: BoundedVec -> BndShape  
bndShape (BoundedVec dyn) = BndShape (Dyn.vecLen dyn - 1) (Dyn.head dyn)

bndVecLen :: BoundedVec -> Int
bndVecLen (BoundedVec dyn) = Dyn.vecLen dyn - 1

bndVecMax :: BoundedVec -> Word
bndVecMax (BoundedVec dyn) = Dyn.head dyn

--------------------------------------------------------------------------------
  
instance Show BoundedVec where
  showsPrec = showsPrecBoundedVec

showBoundedVec :: BoundedVec -> String
showBoundedVec vec = showsPrecBoundedVec 0 vec []

showsPrecBoundedVec :: Int -> BoundedVec -> ShowS
showsPrecBoundedVec prec bndvec
  = showParen (prec > 10) 
  $ showString "fromList' "
  . showsPrec 11 (bndShape bndvec)
  . showChar ' ' 
  . shows (toList bndvec)

--------------------------------------------------------------------------------
-- * Empty
  
empty :: BoundedVec
empty = fromList []

null :: BoundedVec -> Bool
null v = bndVecLen v == 0

--------------------------------------------------------------------------------
-- * Conversion from\/to lists

toList :: BoundedVec -> [Word]
toList (BoundedVec dynvec) = tail $ Dyn.toList dynvec

toListWithMax :: BoundedVec -> (Word,[Word])
toListWithMax (BoundedVec dynvec) = case uncons (Dyn.toList dynvec) of
  Just (x,xs) -> (x,xs)
  Nothing     -> error "BoundedVec/toListWithMax: FATAL ERROR! this should not happen"
  
fromList :: [Word] -> BoundedVec
fromList [] = BoundedVec $ Dyn.fromList' (Dyn.Shape    1  4) [0]
fromList xs = BoundedVec $ Dyn.fromList' (Dyn.Shape (l+1) b) (m:xs) where
  l = length xs
  m = maximum xs
  b = Dyn.bitsNeededFor m

fromList' :: BndShape -> [Word] -> BoundedVec
fromList' (BndShape len max) ws = BoundedVec $ Dyn.fromList' (Dyn.Shape (len+1) bits) ws where
  bits = Dyn.bitsNeededFor max


--------------------------------------------------------------------------------
-- * Indexing

unsafeIndex :: Int -> BoundedVec -> Word
unsafeIndex idx (BoundedVec dynvec) = Dyn.unsafeIndex (idx+1) dynvec 

safeIndex :: Int -> BoundedVec -> Maybe Word
safeIndex idx bndvec@(BoundedVec dynvec)
  | idx < 0    = Nothing
  | idx >= len = Nothing
  | otherwise  = Just $ Dyn.unsafeIndex (idx+1) dynvec
  where
    len = bndVecLen bndvec
    
head :: BoundedVec -> Word
head (BoundedVec dynvec) = Dyn.unsafeIndex 1 dynvec 

--------------------------------------------------------------------------------

concat :: BoundedVec -> BoundedVec -> BoundedVec
concat u v = fromList' (BndShape (lu+lv) (max mu mv)) (toList u ++ toList v) where
  BndShape lu mu = bndShape u
  BndShape lv mv = bndShape v
  
--------------------------------------------------------------------------------
