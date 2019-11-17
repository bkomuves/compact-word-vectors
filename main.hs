
{-# LANGUAGE BangPatterns #-}

import Control.Monad

import Data.List
import qualified Data.Map.Strict as Map

import Dynamic as Dyn

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
nn = 500
117,591,952 bytes maximum residency 
MUT     time    2.390s  (  2.415s elapsed)
GC      time    0.558s  (  0.656s elapsed)
EXIT    time    0.000s  (  0.008s elapsed)
Total   time    2.948s  (  3.081s elapsed)

nn = 1000
695,819,000 bytes maximum residency
MUT     time   17.343s  ( 17.505s elapsed)
GC      time    3.094s  (  3.651s elapsed)
Total   time   20.437s  ( 21.164s elapsed)
-}

tableDyn :: Map.Map DynWordVec Word
tableDyn = Map.fromList  
  [ ( Dyn.fromList list , sum list )  
  | f<-[1..nn]
  , n<-[0..nn]
  , let list = [f..f+n]
  ]

{-
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

{-
  print $ bad1a 300 300
  print $ bad2a 300 300  
-}

  forM_ [1..7 ] $ \i -> print (i , and [ test1a (2^i+o) n | o<-[  -1..100], n<-[1..300] ] )
  forM_ [8..63] $ \i -> print (i , and [ test1a (2^i+o) n | o<-[-100..100], n<-[1..300] ] )
  