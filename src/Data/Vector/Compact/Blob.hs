
-- | Blobs are raw data in continuous regions of memory.
--
-- This library provides a type for blobs consisting 64 bit words 
-- which is optimized for small sizes. They take:
--
-- * only 1 extra word up for blobs of size up to 48 bytes (that is, up to 6 @Word64@-s);
--
-- * but (unfortunataly) 4 extra words above that.
--
-- (This particular tradeoff was chosen so that pointer tagging still
-- works on 64 bit architectures: there are 7 constructors of the data type.)
--
-- The 'Blob' data type is useful if you want to store large amounts of small,
-- serialized data. Some example use cases:
--
--  * small vectors of small nonnegative integers (for example: partitions, permutations, monomials)
-- 
--  * cryptographic hashes 
--
--  * tables indexed by such things
--

{-# LANGUAGE CPP, BangPatterns, MagicHash, UnboxedTuples #-}
module Data.Vector.Compact.Blob where

--------------------------------------------------------------------------------

import Data.Char
import Data.Bits
import Data.Int
import Data.Word
import Data.List

import Control.Monad
import Control.Monad.ST

import GHC.Int
import GHC.Word
import GHC.Ptr
import GHC.Exts
import GHC.IO

import Foreign.Ptr
import Foreign.Marshal.Array

import Control.Monad.Primitive
import Data.Primitive.ByteArray

--------------------------------------------------------------------------------
-- * the Blob type

-- | A 'Blob' is a nonempty array of 'Word64'-s.
-- For arrays of length at most 6 (that is, at most 48 bytes), there is only a single
-- machine word overhead in memory consumption. For larger arrays, there is 4 words of overhead.
--  
data Blob
  = Blob1 {-# UNPACK #-} !Word64
  | Blob2 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  | Blob3 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  | Blob4 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  | Blob5 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  | Blob6 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  | BlobN !(ByteArray#) 

--------------------------------------------------------------------------------

blobTag :: Blob -> Int
blobTag blob = I# (dataToTag# blob)

-- | Number of 'Word64'-s
blobSizeInWords :: Blob -> Int
blobSizeInWords blob = case blob of
  BlobN arr  -> shiftR (I# (sizeofByteArray# arr)) 3
  otherwise  -> blobTag blob + 1

blobSizeInBytes :: Blob -> Int
blobSizeInBytes blob = case blob of
  BlobN arr  -> I# (sizeofByteArray# arr)
  otherwise  -> shiftL (blobTag blob + 1) 3

blobSizeInBits :: Blob -> Int
blobSizeInBits blob = shiftL (blobSizeInBytes blob) 3
    
--------------------------------------------------------------------------------
-- * Conversion to\/from lists

blobFromWordList :: [Word64] -> Blob
blobFromWordList ws = blobFromWordListN (length ws) ws
  
blobFromWordListN :: Int -> [Word64] -> Blob  
blobFromWordListN n ws = case n of
  0 -> Blob1 0
  1 -> case ws of { (a:_)            -> Blob1 a           }
  2 -> case ws of { (a:b:_)          -> Blob2 a b         }
  3 -> case ws of { (a:b:c:_)        -> Blob3 a b c       }
  4 -> case ws of { (a:b:c:d:_)      -> Blob4 a b c d     }
  5 -> case ws of { (a:b:c:d:e:_)    -> Blob5 a b c d e   }
  6 -> case ws of { (a:b:c:d:e:f:_)  -> Blob6 a b c d e f }
  _ -> case byteArrayFromListN n ws of 
         ByteArray ba# -> BlobN ba#
  
blobToWordList :: Blob -> [Word64]
blobToWordList blob = case blob of
  Blob1 a           -> a:[]
  Blob2 a b         -> a:b:[]
  Blob3 a b c       -> a:b:c:[]
  Blob4 a b c d     -> a:b:c:d:[]
  Blob5 a b c d e   -> a:b:c:d:e:[]
  Blob6 a b c d e f -> a:b:c:d:e:f:[]
  BlobN ba#         -> foldrByteArray (:) [] (ByteArray ba#)

--------------------------------------------------------------------------------
-- * Conversion to\/from @ByteArray@-s

-- | Note: we pad the input with zero bytes, assuming little-endian architecture.
blobFromByteArray :: ByteArray -> Blob
blobFromByteArray ba@(ByteArray ba#)
  | nwords >  6  = if nwords1 == nwords
                     then BlobN ba#
                     else let ByteArray new# = byteArrayFromListN nwords words 
                          in  BlobN new#
  | nwords == 0  = Blob1 0
  | otherwise    = blobFromWordListN nwords words
  where
    !nbytes  = sizeofByteArray ba
    !nwords1 = shiftR (nbytes    ) 3 
    !nwords  = shiftR (nbytes + 7) 3

    words :: [Word64]
    words = if nwords1 == nwords
      then foldrByteArray (:) [] (ByteArray ba#)  
      else let !ofs = shiftL nwords1 3
               !m =   nbytes - ofs
               w8_to_w64 :: Word8 -> Word64
               w8_to_w64 = fromIntegral
               !lastWord = foldl' (.|.) 0 [ shiftL (w8_to_w64 (indexByteArray ba (ofs + i))) (shiftL i 3) | i<-[0..m-1] ]
           in  foldrByteArray (:) [] (ByteArray ba#) ++ [lastWord] 

blobToByteArray :: Blob -> ByteArray
blobToByteArray blob = case blob of
  BlobN ba#         -> ByteArray ba#
  _                 -> byteArrayFromListN (blobSizeInWords blob) (blobToWordList blob)

--------------------------------------------------------------------------------
  
instance Show Blob where
  showsPrec prec blob 
    = showParen (prec > 10) 
    $ showString "blobFromWordList " 
    . shows (map Hex $ blobToWordList blob)

instance Eq Blob where
  (==) = eqBlob

eqBlob :: Blob -> Blob -> Bool  
eqBlob !x !y = if blobTag x /= blobTag y 
  then False 
  else case (x,y) of
    ( Blob1 a           , Blob1 p           ) -> a==p
    ( Blob2 a b         , Blob2 p q         ) -> a==p && b==q
    ( Blob3 a b c       , Blob3 p q r       ) -> a==p && b==q && c==r
    ( Blob4 a b c d     , Blob4 p q r s     ) -> a==p && b==q && c==r && d==s
    ( Blob5 a b c d e   , Blob5 p q r s t   ) -> a==p && b==q && c==r && d==s && e==t
    ( Blob6 a b c d e f , Blob6 p q r s t u ) -> a==p && b==q && c==r && d==s && e==t && f==u
    ( BlobN one#        , BlobN two#        ) -> ByteArray one# == ByteArray two#     
    _                                         -> error "FATAL ERROR: should not happen"

--------------------------------------------------------------------------------
-- * Hexadecimal
  
newtype Hex 
  = Hex Word64 

instance Show Hex where 
  show (Hex w) = hexWord64 w 

hexWord64 :: Word64 -> String
hexWord64 word= '0' : 'x' : hexWord64_ word 

hexWord64_ :: Word64 -> String
hexWord64_ word = go [] 16 word where
  
  go !acc  0 !w = acc 
  go !acc !k !w = go (hexNibble (w .&. 15) : acc) (k-1) (shiftR w 4) 
  
  hexNibble :: Integral a => a -> Char
  hexNibble i0 = let i = (fromIntegral i0 :: Int) in if (i < 10) then chr (i+48) else chr (i+87)
      
--------------------------------------------------------------------------------
-- * Peek
 
peekWord :: Blob -> Int -> Word64
peekWord blob idx = case blob of

  Blob1 a  
    | idx == 0   -> a
    | otherwise  -> error "Blob/peekWord: index out of bounds"

  Blob2 a b
    | idx == 0   -> a
    | idx == 1   -> b
    | otherwise  -> error "Blob/peekWord: index out of bounds"

  Blob3 a b c
    | idx == 0   -> a
    | idx == 1   -> b
    | idx == 2   -> c
    | otherwise  -> error "Blob/peekWord: index out of bounds"

  Blob4 a b c d 
    | idx == 0   -> a
    | idx == 1   -> b
    | idx == 2   -> c
    | idx == 3   -> d
    | otherwise  -> error "Blob/peekWord: index out of bounds"

  Blob5 a b c d e 
    | idx == 0   -> a
    | idx == 1   -> b
    | idx == 2   -> c
    | idx == 3   -> d
    | idx == 4   -> e
    | otherwise  -> error "Blob/peekWord: index out of bounds"

  Blob6 a b c d e f
    | idx == 0   -> a
    | idx == 1   -> b
    | idx == 2   -> c
    | idx == 3   -> d
    | idx == 4   -> e
    | idx == 5   -> f
    | otherwise  -> error "Blob/peekWord: index out of bounds"

  BlobN arr# -> indexByteArray (ByteArray arr#) idx
 
-- | NOTE: We assume a little-endian architecture here.
-- Though it seems that since GHC does not gives us direct access to the closure,
-- it doesn\'t matter after all...
--  
peekByte :: Blob -> Int -> Word8
peekByte blob idx =
  let w = peekWord blob (shiftR idx 3)
  in  fromIntegral $ shiftR w (8 * (idx .&. 7))

blobHead :: Blob -> Word64
blobHead blob = case blob of
  Blob1 a             -> a
  Blob2 a _           -> a
  Blob3 a _ _         -> a
  Blob4 a _ _ _       -> a
  Blob5 a _ _ _ _     -> a
  Blob6 a _ _ _ _ _   -> a
  BlobN arr#          -> indexByteArray (ByteArray arr#) 0
  
--------------------------------------------------------------------------------

-- | @extractSmallWord n blob ofs@ extracts a small word of @n@ bits starting from the
-- @ofs@-th bit. This should satisfy
--
-- > testBit (extractSmallWord n blob ofs) i == testBit blob (ofs+i)  
--
-- NOTE: we assume that @n@ is at most the bits in 'Word', and that @ofs+n@ is less
-- than the size (in bits) of the blob.
--
extractSmallWord :: Integral a => Int -> Blob -> Int -> a
extractSmallWord n blob ofs = fromIntegral (extractSmallWord64 n blob ofs)

extractSmallWord64 :: Int -> Blob -> Int -> Word64 
extractSmallWord64 !n !blob !ofs
  | q2 == q1     = mask .&.  shiftR (peekWord blob q1) r1
  | q2 == q1 + 1 = mask .&. (shiftR (peekWord blob q1) r1 .|. shiftL (peekWord blob q2) (64-r1))
  | otherwise    = error "Blob/extractSmallWord: FATAL ERROR"
  where
    mask = shiftL 1 n - 1
    end  = ofs + n - 1
    q1   = shiftR ofs 6 
    q2   = shiftR end 6 
    r1   = ofs .&. 63

-- | An alternate implementation using 'testBit', for testing purposes only
extractSmallWord64_naive :: Int -> Blob -> Int -> Word64     
extractSmallWord64_naive n blob ofs = sum [ shiftL 1 i | i<-[0..n-1] , testBit blob (ofs+i) ]

--------------------------------------------------------------------------------
-- * change size

extendToSize :: Int -> Blob -> Blob
extendToSize tgt blob 
  | n >= tgt   = blob
  | otherwise  = blobFromWordListN tgt (blobToWordList blob ++ replicate (tgt-n) 0)
  where
    n = blobSizeInWords blob

cutToSize :: Int -> Blob -> Blob
cutToSize tgt blob 
  | n <= tgt   = blob
  | otherwise  = blobFromWordListN tgt (take tgt $ blobToWordList blob)
  where
    n = blobSizeInWords blob

forceToSize :: Int -> Blob -> Blob
forceToSize tgt blob 
  | n == tgt   = blob
  | n >= tgt   = blobFromWordListN tgt (take tgt $ blobToWordList blob)
  | otherwise  = blobFromWordListN tgt (blobToWordList blob ++ replicate (tgt-n) 0)
  where
    n = blobSizeInWords blob
    
--------------------------------------------------------------------------------
-- * map and zipWith

mapBlob :: (Word64 -> Word64) -> Blob -> Blob
mapBlob f blob = case blob of
  Blob1 a           -> Blob1 (f a)
  Blob2 a b         -> Blob2 (f a) (f b)
  Blob3 a b c       -> Blob3 (f a) (f b) (f c)
  Blob4 a b c d     -> Blob4 (f a) (f b) (f c) (f d)
  Blob5 a b c d e   -> Blob5 (f a) (f b) (f c) (f d) (f e)
  Blob6 a b c d e y -> Blob6 (f a) (f b) (f c) (f d) (f e) (f y)
  BlobN arr#        -> runST $ do
    let !n = blobSizeInWords blob
    let ba = ByteArray arr#
    mut <- newByteArray (shiftL n 3)
    forM_ [0..n-1] $ \i -> writeByteArray mut i $ f (indexByteArray ba i)
    new@(ByteArray new#) <- unsafeFreezeByteArray mut 
    return (BlobN new#)
   
shortZipWith :: (Word64 -> Word64 -> Word64) -> Blob -> Blob -> Blob 
shortZipWith f !blob1 !blob2 
  | n1 == n2   = unsafeZipWith f               blob1               blob2 
  | n1 >  n2   = unsafeZipWith f (cutToSize n2 blob1)              blob2
  | otherwise  = unsafeZipWith f               blob1 (cutToSize n1 blob2) 
  where
    n1 = blobSizeInWords blob1
    n2 = blobSizeInWords blob2

longZipWith :: (Word64 -> Word64 -> Word64) -> Blob -> Blob -> Blob 
longZipWith f !blob1 !blob2 
  | n1 == n2   = unsafeZipWith f                  blob1                  blob2 
  | n1 <  n2   = unsafeZipWith f (extendToSize n2 blob1)                 blob2
  | otherwise  = unsafeZipWith f                  blob1 (extendToSize n1 blob2) 
  where
    n1 = blobSizeInWords blob1
    n2 = blobSizeInWords blob2

unsafeZipWith :: (Word64 -> Word64 -> Word64) -> Blob -> Blob -> Blob 
unsafeZipWith f !blob1 !blob2 = case (blob1,blob2) of
  ( Blob1 a           , Blob1 p           ) -> Blob1 (f a p)
  ( Blob2 a b         , Blob2 p q         ) -> Blob2 (f a p) (f b q)
  ( Blob3 a b c       , Blob3 p q r       ) -> Blob3 (f a p) (f b q) (f c r)
  ( Blob4 a b c d     , Blob4 p q r s     ) -> Blob4 (f a p) (f b q) (f c r) (f d s)
  ( Blob5 a b c d e   , Blob5 p q r s t   ) -> Blob5 (f a p) (f b q) (f c r) (f d s) (f e t)
  ( Blob6 a b c d e y , Blob6 p q r s t u ) -> Blob6 (f a p) (f b q) (f c r) (f d s) (f e t) (f y u)
  ( BlobN one#        , BlobN two#        ) -> 
      runST $ do
        let !n = blobSizeInWords blob1
            ba1 = ByteArray one#
            ba2 = ByteArray two#
        mut <- newByteArray (shiftL n 3)
        forM_ [0..n-1] $ \i -> writeByteArray mut i $ f (indexByteArray ba1 i) (indexByteArray ba2 i)
        new@(ByteArray new#) <- unsafeFreezeByteArray mut 
        return (BlobN new#)
  _ -> error "FATAL ERROR: should not happen"

--------------------------------------------------------------------------------

instance Bits Blob where
  (.&.) = shortZipWith (.&.)
  (.|.) = longZipWith  (.|.) 
  xor   = longZipWith  xor
  complement = mapBlob complement

  shiftL  = blobShiftL -- error "shiftR"
  shiftR  = blobShiftR -- error "shiftR"
  rotateL = error "Blob/rotateL: not implemented"
  rotateR = error "Blob/rotateR: not implemented"

#if MIN_VERSION_base(4,12,0)
  bitSizeMaybe = Just . blobSizeInBits
  bitSize      = blobSizeInBits
#else
  bitSize = blobSizeInBits
#endif

  zeroBits = Blob1 0
  isSigned _    = False
  popCount blob = foldl' (+) 0 (map popCount $ blobToWordList blob) 

  testBit !blob !k = if q >= n then False else testBit (peekWord blob q) r where
    (q,r) = divMod k 64
    n = blobSizeInWords blob

  bit k = blobFromWordListN (q+1) (replicate q 0 ++ [bit r]) where 
    (q,r) = divMod k 64

#if MIN_VERSION_base(4,12,0)
instance FiniteBits Blob where
  finiteBitSize = blobSizeInBits
#endif

--------------------------------------------------------------------------------
-- * Cons, Snoc, tail

-- | Add a word at the start
blobConsWord :: Word64 -> Blob -> Blob
blobConsWord !y !blob = case blob of
  Blob1 a           -> Blob2 y a
  Blob2 a b         -> Blob3 y a b
  Blob3 a b c       -> Blob4 y a b c
  Blob4 a b c d     -> Blob5 y a b c d
  Blob5 a b c d e   -> Blob6 y a b c d e
  Blob6 a b c d e f -> BlobN new# where ByteArray new# = byteArrayFromListN 7 [y,a,b,c,d,e,f]
  BlobN ba#         -> BlobN new# where ByteArray new# = byteArrayFromListN (n+1) (y : baToList# ba#) 
                                        n = baSizeInWords# ba#

-- | Add a word at the end
blobSnocWord :: Blob -> Word64 -> Blob
blobSnocWord !blob !z = case blob of
  Blob1 a           -> Blob2 a z
  Blob2 a b         -> Blob3 a b z
  Blob3 a b c       -> Blob4 a b c z
  Blob4 a b c d     -> Blob5 a b c d z
  Blob5 a b c d e   -> Blob6 a b c d e z
  Blob6 a b c d e f -> BlobN new# where ByteArray new# = byteArrayFromListN 7 [a,b,c,d,e,f,z]
  BlobN ba#         -> BlobN new# where ByteArray new# = byteArrayFromListN (n+1) (foldrByteArray (:) [z] (ByteArray ba#))
                                        n = baSizeInWords# ba#
-- | Remove the first word
blobTail :: Blob -> Blob 
blobTail !blob = case blob of
  Blob1 _           -> Blob1 0
  Blob2 _ b         -> Blob1 b 
  Blob3 _ b c       -> Blob2 b c
  Blob4 _ b c d     -> Blob3 b c d 
  Blob5 _ b c d e   -> Blob4 b c d e 
  Blob6 _ b c d e f -> Blob5 b c d e f 
  BlobN ba#         -> 
    let !n = baSizeInWords# ba#
    in  if n == 7 
          then let [b,c,d,e,f,g]  = tail (baToList# ba#)
               in  Blob6 b c d e f g 
          else let ByteArray new# = byteArrayFromListN (n-1) (tail (baToList# ba#))
               in  BlobN new#                        

--------------------------------------------------------------------------------
-- * Shift left, shift right

blobShiftL :: Blob -> Int -> Blob
blobShiftL blob  0 = blob
blobShiftL blob !n = flip shiftL_small r $ go q blob where
  (q,r) = divMod n 64
  go !0  blob = blob
  go !k !blob = go (k-1) (shiftL_by64 blob)

blobShiftR :: Blob -> Int -> Blob
blobShiftR blob  0 = blob
blobShiftR blob !n = flip shiftR_small r $ go q blob where
  (q,r) = divMod n 64
  go !0  blob = blob
  go !k !blob = go (k-1) (shiftR_by64 blob)

-- | shifts lefts, and uses the given word the fill the resulting 0 bits
shiftLeftAndFill :: Blob -> Int -> Word64 -> Blob
shiftLeftAndFill blob 0  word = blob 
shiftLeftAndFill blob !n word = go q (shiftL_small_and_fill blob r word)     -- incorrect!!! consider 65 bits
  where
    (q,r) = divMod n 64
    go !0  blob = blob
    go !k !blob = go (k-1) (shiftL_by64 blob)
  
-- | Shift left by 64 bits
shiftL_by64 :: Blob -> Blob
shiftL_by64 = blobConsWord 0

-- | Shift right by 64 bits
shiftR_by64 :: Blob -> Blob
shiftR_by64 = blobTail

-- | Shift left by less than 64 bits
shiftL_small :: Blob -> Int -> Blob
shiftL_small blob k = shiftL_small_and_fill blob k 0 

-- | Shift left by less than 64 bits, and fills the resulting 0 bits using the given word
shiftL_small_and_fill :: Blob -> Int -> Word64-> Blob
shiftL_small_and_fill !blob  0  _    = blob
shiftL_small_and_fill !blob !k !word = 

  case blob of
    Blob1 a            -> Blob2  (shiftL a k .|. w1) (shiftR a r)
    Blob2 a b          -> Blob3  (shiftL a k .|. w1) (shiftR a r .|. shiftL b k) (shiftR b r)
    Blob3 a b c        -> Blob4  (shiftL a k .|. w1) (shiftR a r .|. shiftL b k) (shiftR b r .|. shiftL c k) (shiftR c r)
    Blob4 a b c d      -> Blob5  (shiftL a k .|. w1) (shiftR a r .|. shiftL b k) (shiftR b r .|. shiftL c k) (shiftR c r .|. shiftL d k) (shiftR d r)
    Blob5 a b c d e    -> Blob6  (shiftL a k .|. w1) (shiftR a r .|. shiftL b k) (shiftR b r .|. shiftL c k) (shiftR c r .|. shiftL d k) (shiftR d r .|. shiftL e k) (shiftR e r)
    Blob6 a b c d e f  -> let l=[(shiftL a k .|. w1),(shiftR a r .|. shiftL b k),(shiftR b r .|. shiftL c k),(shiftR c r .|. shiftL d k),(shiftR d r .|. shiftL e k),(shiftR e r .|. shiftL f k),(shiftR f r)]
                              ByteArray new# = byteArrayFromListN 7 l
                          in  BlobN new#                             
  
    BlobN ba# -> BlobN new# where     
      !n = baSizeInWords# ba# 
      ByteArray new# = byteArrayFromListN (n+1) 
                     $ worker (shiftL word r) (baToList# ba#)

  where
    !r = 64 - k  
    mask = shiftL 2 k - 1
    w1   = word .&. mask

    worker !prev (x:xs) = (shiftL x k .|. shiftR prev r) : worker x xs
    worker !prev []     = (               shiftR prev r) : []

-- | Shift right by less than 64 bits
shiftR_small :: Blob -> Int -> Blob
shiftR_small !blob  0 = blob
shiftR_small !blob !k = 

  case blob of                  
    Blob1 a            -> Blob1 (shiftR a k)
    Blob2 a b          -> Blob2 (shiftR a k .|. shiftL b r) (shiftR b k)
    Blob3 a b c        -> Blob3 (shiftR a k .|. shiftL b r) (shiftR b k .|. shiftL c r) (shiftR c k)
    Blob4 a b c d      -> Blob4 (shiftR a k .|. shiftL b r) (shiftR b k .|. shiftL c r) (shiftR c k .|. shiftL d r) (shiftR d k)
    Blob5 a b c d e    -> Blob5 (shiftR a k .|. shiftL b r) (shiftR b k .|. shiftL c r) (shiftR c k .|. shiftL d r) (shiftR d k .|. shiftL e r) (shiftR e k)
    Blob6 a b c d e f  -> Blob6 (shiftR a k .|. shiftL b r) (shiftR b k .|. shiftL c r) (shiftR c k .|. shiftL d r) (shiftR d k .|. shiftL e r) (shiftR e k .|. shiftL f r) (shiftR f k)
    BlobN ba#          -> BlobN new# where     
      !n = baSizeInWords# ba# 
      ByteArray new# = case baToList# ba# of (x:xs) -> byteArrayFromListN n (worker x xs)

  where
    !r = 64 - k  

    worker !this (y:ys) = (shiftR this k .|. shiftL y r) : worker y ys
    worker !this []     = (shiftR this k               ) : []

--------------------------------------------------------------------------------
-- * ByteArray helpers

baToList :: ByteArray -> [Word64]
baToList = foldrByteArray (:) [] 

baToList# :: ByteArray# -> [Word64]
baToList# ba# = foldrByteArray (:) [] (ByteArray ba#)

baSizeInWords :: ByteArray -> Int
baSizeInWords ba = shiftR (sizeofByteArray ba) 3

baSizeInWords# :: ByteArray# -> Int
baSizeInWords# ba# = shiftR (I# (sizeofByteArray# ba#)) 3

--------------------------------------------------------------------------------
