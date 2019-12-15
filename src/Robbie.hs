module Robbie (
  makeRW
) where

import System.Random(randomRs, randomR, getStdRandom, StdGen, Random, RandomGen)
import Data.Array.IO(IOUArray, IOArray)
import Data.Array.MArray(readArray, writeArray, newArray, newArray_, newListArray)
import Data.Word(Word8)
import Data.Ratio(approxRational, numerator, denominator)
import Control.Monad(guard, forM_)
import Data.IntMap.Strict(IntMap)
import qualified Data.IntMap as IM
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe(catMaybes)

mistakePenalty :: Int
mistakePenalty = 5

rewardCollect :: Int
rewardCollect = 1

data Sim = Wrap RobbieWorld Genome RobbieState
type Action = (Sim -> IO Sim)
type RobbieWorld = IOArray Int (IOUArray Int Word8) 
type Genome = IntMap Action 
type RobbieState = IOUArray Word8 Int 

act :: Sim -> IO Sim
act sim@(Wrap rw gnm rs) = do
    sur <- sense rw rs 
    case IM.lookup sur gnm of
        (Just step) -> step sim
        Nothing     -> error "act: state not found in genome: this should not ever occur"
      
sense :: RobbieWorld -> RobbieState -> IO Int
sense rw rs = do 
    x <- readArray rs 1
    y <- readArray rs 2
    hashLoc <$> access2d (x,y) rw
      <*> access2d (x+1, y) rw
      <*> access2d (x-1, y) rw
      <*> access2d (x, y+1) rw
      <*> access2d (x, y-1) rw

access2d :: (Int, Int) -> RobbieWorld -> IO Word8
access2d (x, y) rw = do
    row <- readArray rw x
    readArray row y

mvRand :: Sim -> IO Sim
mvRand sim@(Wrap rw gnm rs) = do
    r <- getStdRandom $ randomR (2,5) 
    case IM.lookup r gnm of
        (Just go) -> go sim
        Nothing   -> error "mvRand: received value not in genome: absolutely should not happen"
      
collect :: Sim -> IO Sim
collect sim@(Wrap rw gnm rs) = do
    x <- readArray rs 1
    y <- readArray rs 2
    h <- access2d (x,y) rw
    if h /= 2
      then return sim
      else do
        row <- readArray rw x
        writeArray row y 1
        score <- readArray rs 0
        writeArray rs 0 $ score + rewardCollect
        return $ Wrap rw gnm rs

stay :: Sim -> IO Sim
stay sim = return sim

north :: Sim -> IO Sim
north = move (1,0)
      
south :: Sim -> IO Sim
south = move (-1,0)
 
east :: Sim -> IO Sim 
east = move (0,1)
     
west :: Sim -> IO Sim
west = move (0,-1)

move :: (Int, Int) -> Sim -> IO Sim
move (i,j) (Wrap rw gnm rs) = do
    let idx = if i == 0 then 2 else 1
    ent <- fetchRel (i,j) rs rw 
    if ent /= 0
      then do cur <- readArray rs idx
              writeArray rs idx $ cur + i + j
              return $ Wrap rw gnm rs
      else do cur <- readArray rs 0
              writeArray rs 0 $ cur - mistakePenalty
              return $ Wrap rw gnm rs

fetchRel :: (Int, Int) -> RobbieState -> RobbieWorld -> IO Word8
fetchRel (i, j) rs rw = do
    x <- readArray rs 1
    y <- readArray rs 2
    access2d (x + i, y + j) rw

mkRobbieState :: (RandomGen g) => Int -> g -> IO RobbieState 
mkRobbieState n g = newListArray (0,2) es where
    es = 0 : (rs ++ [0])
    rs = take 2 $ randomRs (1,n) g

mkGenome :: (RandomGen g) => g -> Genome
mkGenome g = IM.fromList $ flip zip actions $ do 
      let vs = [0..2]
      n <- vs
      s <- vs
      e <- vs
      w <- vs
      h <- vs
      guard (h /= 0)
      guard ((length $ filter (==0) [n, s, e, w, h]) < 3)
      return $ hashLoc h s n e w
    where actions = catMaybes $ map (flip M.lookup actMap) $ randomRs (1,7) g
        
actMap :: Map Word8 Action
actMap = M.fromList $ zip [1..7] [mvRand, north, south, east, west, stay, collect]

hashLoc :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Int
hashLoc h n s e w = sum $ zipWith (*) integralLoc powersOf3 where
    integralLoc = map fromIntegral [h,n,s,e,w]
    powersOf3 = map (3^) [1..]

makeRW :: (RealFrac c) => Int -> c -> c -> IO RobbieWorld
makeRW n frac eps = do 
    outer <- newArray_ (0,n+1) 
    forM_ [0..n+1] $ \i -> do
      row <- newArray_ (0,n+1)
      writeArray outer i row
      if i == 0 || i == n + 1
        then do 
            forM_ [0..n+1] $ \i -> writeArray row i 0
        else do
            writeArray row 0 0
            writeArray row (n+1) 0
            forM_ [1..n] $ \i -> do
              r <- shift nm <$> getStdRandom (randomR (1,dnm))
              writeArray row i r
    return outer
  where
    shift s x = if x > s then 1 else 2
    nm  = numerator $ approxRational frac eps
    dnm = denominator $ approxRational frac eps

