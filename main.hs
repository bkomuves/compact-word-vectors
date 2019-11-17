
{-# LANGUAGE BangPatterns #-}

import Control.Monad

import Data.List
import qualified Data.Map.Strict as Map

import Blob
import Dynamic as Dyn

import GHC.DataSize

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

nn = 500

{-
nn = 250
16,104,664 bytes maximum residency
MUT     time    0.298s  (  0.302s elapsed)
GC      time    0.101s  (  0.114s elapsed)
Total   time    0.400s  (  0.425s elapsed)
  
nn = 500
109,637,536 bytes maximum residency
MUT     time    2.376s  (  2.419s elapsed)
GC      time    0.547s  (  0.648s elapsed)
Total   time    2.922s  (  3.079s elapsed)

nn = 1000
617,839,680 bytes maximum residency
MUT     time   17.070s  ( 17.254s elapsed)
GC      time    3.133s  (  3.677s elapsed)
Total   time   20.204s  ( 20.934s elapsed)
-}
tableDyn :: Map.Map DynWordVec Word
tableDyn = Map.fromList  
  [ ( Dyn.fromList list , sum list )  
  | f<-[1..nn]
  , n<-[0..nn]
  , let list = [f..f+n]
  ]

{-
nn = 250
308,261,800 bytes maximum residency 
MUT     time    0.190s  (  0.192s elapsed)
GC      time    0.713s  (  0.927s elapsed)
Total   time    0.903s  (  1.130s elapsed)

nn = 500
2,308,906,360 bytes maximum residency 
MUT     time    1.401s  (  1.418s elapsed)
GC      time    5.214s  (  6.603s elapsed)
Total   time    6.615s  (  8.035s elapsed)
-}
tableList :: Map.Map [Word] Word
tableList = Map.fromList  
  [ ( list , sum list )  
  | f<-[1..nn]
  , n<-[0..nn]
  , let list = [f..f+n]
  ]
  
--------------------------------------------------------------------------------

main = do
{-
  print $ foldl' (+) 0 
        $ map (fromIntegral :: Word -> Integer)  
        $ map snd 
        $ Map.toList tableDyn

-}
  print $ bad1a 300 300
  print $ bad2a 300 300  

  forM_ [1..7 ] $ \i -> print (i , and [ test1a (2^i+o) n | o<-[  -1..100], n<-[1..300] ] )
  forM_ [8..63] $ \i -> print (i , and [ test1a (2^i+o) n | o<-[-100..100], n<-[1..300] ] )
  
  let a = fromList [  1.. 14]
      b = fromList [101..114]
  print a
  print b
  print =<< ( a  `seq` recursiveSize a  )
  print =<< ( b  `seq` recursiveSize b  )
