module Robbie (
  mkGrid,
  mkSubGrid,
) where

import System.Random(randomRs, mkStdGen, split, StdGen, Random, RandomGen)
import Data.List(splitAt)
import Data.Array.IArray(IArray, Array, Ix, array)
import Data.Array.Unboxed(UArray)
import Data.Word(Word8)

{-mkSqGrid :: Int -> Int -> Array Int (UArray Int Word8)    
mkSqGrid seed dim = runSTArray $ do
    newArray(0, dim + 1) $ runSTUArray $ do 
        newArray(0, dim + 1) 0
-}
mkGrid :: (IArray a (b Int e), IArray b e, Num e, Random e, RandomGen g) =>
          Int -> g 
            -> a Int (b Int e)
mkGrid n g = array (0,n+1) subGrid where
    subGrid = zip [0..n+1] $ mkSubGrid n $ randomRs (1,2) g

mkSubGrid :: (IArray a e, Num e) => Int -> [e] -> [a Int e]
mkSubGrid n xs@(_:_) = map (mkArray n) $ replicate (n+2) 0 : mkMidRows n xs where
    mkMidRows :: (Num a) => Int -> [a] -> [[a]]
    mkMidRows n xs@(_:_) = gs : mkMidRows n rs where
        gs = 0 : (g ++ [0])
        (g, rs) = splitAt n xs
    mkMidRows n [] = replicate (n+2) 0 : []
    mkArray :: (IArray a e, Ix i, Num i, Enum i) => i -> [e] -> a i e
    mkArray n = array (0,n+1) . zip [0..n+1]


