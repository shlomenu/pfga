module Robbie (
   Sim
 , Genome 
 , mkSimWithGenome
 , mkGenome
 , act
 , crossGenomes
 , mutateGenome
 , readScore
 , ratioFromFloat
) where

-----------------------------------------------------------------

{- 
      function imports: 
-}

import System.Random( randomRs, randomR, 
                      getStdRandom, randomRIO, 
                      newStdGen )
import Data.Array.MArray( readArray, writeArray, 
                          newArray_, newListArray )
import Data.Ratio( approxRational, numerator, denominator )
import Control.Monad( guard, forM_ )
import Data.Maybe( catMaybes, fromJust )
import Data.List( foldl' )

{- 
      type imports: 
-}

import Data.Array.IO(IOUArray, IOArray)
import Data.Word(Word8)
import Data.IntMap.Strict(IntMap)
import Data.Map(Map)
import Control.DeepSeq(NFData(..))

{-
      qualified imports:
-}

import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.IntSet as IS

-----------------------------------------------------------------
{- Types, Instances, Type Synonyms -}

data Sim = Wrap RobbieWorld Genome RobbieState
data Label = MvRand 
           | North 
           | South 
           | East 
           | West 
           | Stay 
           | Collect 
  deriving (Eq, Ord, Show)

instance NFData Label where
  rnf a = a `seq` ()

instance NFData Sim where
  rnf (Wrap rw gnm rs) = rnf gnm `seq` rw `seq` rs `seq` ()

type Action = (Sim -> IO Sim)
type RobbieWorld = IOArray Int (IOUArray Int Word8) 
type Genome = IntMap Label 
type RobbieState = IOUArray Word8 Int 

-----------------------------------------------------------------
{- Constants -}

mistakePenalty :: Int
mistakePenalty = 5

rewardCollect :: Int
rewardCollect = 1

globalEps :: (RealFrac c) => c
globalEps = 0.001

labels :: [Label]
labels = [MvRand, North, South, East, West, Stay, Collect]

actions :: [Action]
actions = [mvRand, north, south, east, west, stay, collect]

hashes :: [Int]
hashes = do 
  let vs = [0..2]
  n <- vs
  s <- vs
  e <- vs
  w <- vs
  h <- vs
  guard (h /= 0)
  guard ((length $ filter (==0) [n, s, e, w, h]) < 3)
  return $ hashLoc h s n e w

actMap :: Map Label Action
actMap = M.fromList $ zip labels actions
        
labelMap :: IntMap Label
labelMap = IM.fromList $ zip [1..7] labels

rLabelMap :: Map Label Int
rLabelMap = M.fromList $ zip labels [1..7]

-----------------------------------------------------------------
{- Evolution -}

mutateGenome :: (RealFrac c) => c -> Genome -> IO Genome
mutateGenome frac gnm = do
    let size = IM.size gnm
        (nm, dnm) = ratioFromFloat frac  
    
    rs <- sequence $ replicate size $ randomRIO (1,dnm)
    g <- newStdGen
    
    let keys = map snd $ filter dropRatio $ zip rs $ IM.keys gnm 
        dropRatio (r,_) = if r > nm then False else True
        vs = noNothingLkup rLabelMap M.lookup $ noNothingLkup gnm IM.lookup keys
        mutns = mapMutants vs $ randomRs (1,7) g
        newIMap = foldl' mdf gnm $ zip keys $ noNothingLkup labelMap IM.lookup mutns
    
    return newIMap

mapMutants :: (Eq a) => [a] -> [a] -> [a]
mapMutants e@(b:bs) (c:cs) | c /= b    = c : mapMutants bs cs
                           | otherwise = mapMutants e cs
mapMutants [] _ = []
mapMutants _ [] = []

mdf :: Genome -> (Int, Label) -> Genome
mdf gnm (k,a) = IM.update (\_ -> Just a) k gnm

crossGenomes :: Genome -> Genome -> IO (Genome, Genome)
crossGenomes gnmA gnmB = do 
    let n = IM.size gnmA
    r <- randomRIO (1, n - 1)
    let part = IS.fromAscList $ take r $ IM.keys gnmA
        (btmA, topA) = IM.partitionWithKey (\k _ -> IS.member k part) gnmA
        (btmB, topB) = IM.partitionWithKey (\k _ -> IS.member k part) gnmB
    return (btmA `IM.union` topB, btmB `IM.union` topA)

-----------------------------------------------------------------
{- Stepping Simulations Forward -}

act :: Action
act sim@(Wrap rw gnm rs) = do
    sur <- sense rw rs 
    let step = fromJust $ M.lookup lbl actMap
        lbl = fromJust $ IM.lookup sur gnm
    step sim
      
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

mvRand :: Action
mvRand sim = do
    r <- getStdRandom $ randomR (2,5) 
    let go = fromJust $ M.lookup lbl actMap
        lbl = fromJust $ IM.lookup r labelMap
    go sim
      
collect :: Action
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

stay :: Action
stay sim = return sim

north :: Action
north = move (1,0)
      
south :: Action
south = move (-1,0)
 
east :: Action
east = move (0,1)
     
west :: Action
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

-----------------------------------------------------------------
{- "Constructors" -}

mkSimWithGenome :: Int -> Float -> Genome -> IO Sim
mkSimWithGenome n frac gnm = do
  rw <- makeRW n frac
  rs <- mkRobbieState n
  return $ Wrap rw gnm rs

mkRobbieState :: Int -> IO RobbieState 
mkRobbieState n = do 
    rs <- sequence $ replicate 2 $ randomRIO (1,n)
    let es = 0 : (rs ++ [0])
    newListArray (0,2) es

mkGenome :: IO Genome
mkGenome = do
  rs <- sequence $ replicate (length hashes) $ randomRIO (1,7)
  let gnm = IM.fromList $ zip hashes lbls 
      lbls = noNothingLkup labelMap IM.lookup rs
  return gnm

makeRW :: (RealFrac c) => Int -> c -> IO RobbieWorld
makeRW n frac = do 
    outer <- newArray_ (0,n+1) 
    forM_ [0..n+1] $ \i -> do
      row <- newArray_ (0,n+1)
      writeArray outer i row
      if i == 0 || i == n + 1
        then do 
            forM_ [0..n+1] $ \j -> writeArray row j 0
        else do
            writeArray row 0 0
            writeArray row (n+1) 0
            forM_ [1..n] $ \j -> do
              r <- shift nm <$> getStdRandom (randomR (1,dnm))
              writeArray row j r
    return outer
  where
    shift s x = if x > s then 1 else 2
    (nm, dnm) = ratioFromFloat frac

-----------------------------------------------------------------
{- Utilities & Abbreviations -}

noNothingLkup :: b -> (a -> b -> Maybe c) -> [a] -> [c]
noNothingLkup m lkup ks = catMaybes $ map (flip lkup m) ks

hashLoc :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Int
hashLoc h n s e w = sum $ zipWith (*) integralLoc powersOf3 where
    integralLoc = map fromIntegral [h,n,s,e,w]
    powersOf3 = iterate (3*) 1

readScore :: Sim -> IO Int
readScore (Wrap _ _ rs) = readArray rs 0

ratioFromFloat :: (RealFrac c) => c -> (Integer, Integer)
ratioFromFloat frac = (numerator rt, denominator rt) 
  where rt = approxRational frac globalEps

-----------------------------------------------------------------
-----------------------------------------------------------------
                    {-     Fin      -}
-----------------------------------------------------------------
-----------------------------------------------------------------





