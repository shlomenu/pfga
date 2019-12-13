module Robbie (
  makeRW
) where

import System.Random(randomRs, mkStdGen, split, StdGen, Random, RandomGen)
import Data.List(splitAt)
import Data.Array.IArray(IArray, Array, Ix, array)
import Data.Array.Unboxed(UArray)
import Data.Word(Word8)
import Data.Ratio(approxRational, numerator, denominator)


type RobbieWorld = Array Int (UArray Int Word8)

makeRW :: (RandomGen g, RealFrac c) => Int -> c -> c -> g -> RobbieWorld
makeRW n frac eps g = mkArray n $ map (mkArray n) $ mkSquareGrid n 0 rs where
    rs = take (n*n) $ map (shift nm) $ randomRs (1, dnm) g
    shift s x = if x > s then 1 else 2
    nm = numerator $ approxRational frac eps
    dnm = denominator $ approxRational frac eps

mkSquareGrid :: (Num e) => Int -> e -> [e] -> [[e]]
mkSquareGrid n endfix xs@(_:_) = replicate (n+2) endfix : mkElse n endfix xs 

mkElse :: (Num e) => Int -> e -> [e] -> [[e]]
mkElse n endfix xs@(_:_) = gs : mkElse n endfix rs where
    gs = endfix : (g ++ [endfix])
    (g, rs) = splitAt n xs
mkElse n endfix []       = replicate (n+2) endfix : []

mkArray :: (IArray a e, Ix i, Num i, Enum i) => i -> [e] -> a i e
mkArray n = array (0,n+1) . zip [0..n+1]
