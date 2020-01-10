module Main where

-----------------------------------------------------------------

import Robbie

{- 
      function imports: 
-}

import System.Random( randomRIO )
import System.Environment( getProgName, getArgs )
import System.IO( openFile, hClose, hPutStrLn )
import Data.List( partition, isPrefixOf, sortBy )
import Data.Time.Clock( getCurrentTime )
import Data.Time.LocalTime( getCurrentTimeZone, utcToLocalTime )
import Data.Array( array )
import Data.Random( runRVar )
import Data.Random.Extras( choicesArray )
import Data.Maybe( catMaybes )
import Control.Monad( sequence )
import Control.Monad.Loops( concatM )
import Control.Monad.Par.IO()
import Control.Monad.Par.IO( runParIO )
import Control.Monad.Par.Combinator( parMapM )
import Control.Monad.Trans( liftIO )

{- 
      type imports: 
-}

import System.IO(Handle, IOMode(..))
import Data.Random(StdRandom(..))

{-
      qualified imports:
-}

import qualified Data.IntMap as IM

-----------------------------------------------------------------
{- Constants -}

sampleSize :: Int
sampleSize = 10

-----------------------------------------------------------------
{- Main & Helpers -}

main :: IO ()
main = do
    pn <- getProgName
    (flags, params) <- fmap (partition (isPrefixOf "-")) getArgs    
    let (psize, dim, nStep, nGen, canDensity,
             mutRate, crossRate, logFreq, logFile) = parseParams pn params
        evolve = case filter (=="--par") flags of 
                    ["--par"] -> evolveP
                    _         -> evolveS
    h <- openFile logFile AppendMode
    ut <- getCurrentTime
    tz <- getCurrentTimeZone
    let header = show (utcToLocalTime tz ut) ++ ": " ++
                    pn ++ " " ++ unwords params ++ " " ++ unwords flags
    hPutStrLn h header
    gnms <- initGenomes psize
    let ev = evolve dim nStep logFreq canDensity mutRate crossRate h
    _ <- iterateNM1 ev nGen ((0, nGen), gnms)
    hClose h

parseParams :: String -> [String] -> (Int,Int,Int,Int,Float,Float,Float,Int,String)
parseParams pn (ps:dim:ns:ng:cd:mr:cr:lf:fp:_) | test      = rd
                                               | otherwise = err pn 
    where rd :: (Int,Int,Int,Int,Float,Float,Float,Int,String)
          rd = (read ps, read dim, read ns, read ng, read cd, read mr, read cr, read lf, fp)
          test = and $ (map (>0) [p,d,s,g,l]) ++ (map (\x -> x > 0 && x <= 1) [c,m,o])
            where (p,d,s,g,c,m,o,l,_) = rd
parseParams pn _ = err pn

err :: [Char] -> a
err pn = error $ "usage: " ++ pn ++ " " ++ errString where 
    errString = "pop-size dim nstep ngen can-density mutn-rate crossover-rate log-frequency " ++
                "log-file --par\n" ++ "                  " ++ 
                ":: Int Int Int Int Float Float Float Int FilePath"

initGenomes :: Int -> IO [Genome]
initGenomes = sequence . flip replicate mkGenome

-----------------------------------------------------------------
{- Core Step of the Algorithm (Sequential and Parallel) plus a helper -}

evolveS :: Int -> Int -> Int -> Float -> Float -> Float -> Handle
                  -> ((Int, Int), [Genome]) -> IO ((Int, Int), [Genome])
evolveS dim nstep lf dens mutr crossr h ((i, lastRun), gnms) = do
    let initScores = replicate (length gnms) 0
    scores <- iterateNM3 initScores sampleSize $ \scores' -> do
        sims <- mapM (mkSimWithGenome dim dens) gnms
        steppdSims <- iterateNM2 nstep (\sms -> mapM act sms) sims
        newScores <- mapM readScore steppdSims
        return $ zipWith (+) scores' newScores
    let sortedGnms = sortBy (\(j,_) (k,_) -> compare k j) $ zip scores gnms
        logMsg = show i ++ ": average, " ++ show avgScore ++ "; "
                     ++ "top, " ++ show topScore ++ "; "
                     ++ "approx. median, " ++ show medScore ++ "."
        avgScore = sum sortScores `div` length sortScores
        topScore = head sortScores
        medScore = sortScores !! (length sortScores `quot` 2)
        sortScores = map fst sortedGnms
        l = length sortedGnms
    if i `mod` lf == 0 || i == lastRun 
        then hPutStrLn h logMsg
        else return ()
    selGnms <- rankSel $ zip [l,l-1..1] $ snd $ unzip sortedGnms
    mutants <- mapM (mutateGenome mutr) selGnms
    let (crssNm, crssDnm) = ratioFromFloat crossr
        cross r = if r <= crssNm then uncurry crossGenomes else return
    crossDraw <- sequence $ replicate (length mutants `quot` 2) $ randomRIO (1,crssDnm)
    exchanged <- fmap mapUnpair $ sequence $ zipWith cross crossDraw $ mapPair mutants 
    let newGnms = if even (length mutants) then exchanged else (last mutants) : exchanged
    return $ if i /= lastRun 
               then ((i+1,lastRun), newGnms) 
               else ((lastRun,lastRun), map snd sortedGnms)

evolveP :: Int -> Int -> Int -> Float -> Float -> Float -> Handle
                   -> ((Int, Int), [Genome]) -> IO ((Int, Int), [Genome])
evolveP dim nstep lf dens mutr crossr h ((i,lastRun), gnms) = do
    let initScores = replicate (length gnms) 0
    scores <- iterateNM3 initScores sampleSize $ \scores' -> do
        sims <- runParIO $ parMapM (liftIO . mkSimWithGenome dim dens) gnms
        steppdSims <- runParIO $ parMapM (liftIO . iterateNM2 nstep act) sims
        newScores <- runParIO $ parMapM (liftIO . readScore) steppdSims
        return $ zipWith (+) scores' newScores
    let sortedGnms = sortBy (\(j,_) (k,_) -> compare k j) $ zip scores gnms
        logMsg = show i ++ ": average, " ++ show avgScore ++ "; "
                        ++ "top, " ++ show topScore ++ "; "
                        ++ "approx. median, " ++ show medScore ++ "."
        avgScore = sum sortScores `div` length sortScores
        topScore = head sortScores
        medScore = sortScores !! (length sortScores `quot` 2)
        sortScores = map fst sortedGnms
        l = length sortedGnms
    if i `mod` lf == 0 || i == lastRun 
        then hPutStrLn h logMsg
        else return ()
    selGnms <- rankSel $ zip [l,l-1..1] $ snd $ unzip sortedGnms
    mutants <- runParIO $ parMapM (liftIO . mutateGenome mutr) selGnms
    let (crssNm, crssDnm) = ratioFromFloat crossr
        cross (r, (g1,g2)) = if r <= crssNm then crossGenomes g1 g2 else return (g1,g2)
    crossDraw <- sequence $ replicate (length mutants `quot` 2) $ randomRIO (1,crssDnm)
    let rsWMuts = zip crossDraw (mapPair mutants)
    exchanged <- fmap mapUnpair $ runParIO $ parMapM (liftIO . cross) rsWMuts
    let newGnms = if even (length mutants) then exchanged else (last mutants) : exchanged
    return $ if i /= lastRun 
               then ((i+1,lastRun), newGnms) 
               else ((lastRun,lastRun), map snd sortedGnms)

rankSel :: [(Int, a)] -> IO [a]
rankSel rankedIt = do 
    chs <- flip runRVar StdRandom $ choicesArray (length rankedIt) opts
    return $ catMaybes $ map (flip IM.lookup m . flip mod top) chs
  where
    m = IM.fromList rankedIt
    is = map fst rankedIt
    top = 1 + head is
    nmods n = take n [ x + n | x <- [0,top..] ]  
    ns = zip [1..] $ concat $ map nmods is
    opts = array (1, length ns) ns
    
-----------------------------------------------------------------
{- Monadic Combinators & Utilities -}

mapPair :: [a] -> [(a,a)]
mapPair (a:b:rs) = (a,b) : mapPair rs
mapPair _ = []

mapUnpair :: [(a,a)] -> [a]
mapUnpair ((a,b):rs) = a : b : mapUnpair rs
mapUnpair [] = []

iterateNM1 :: (Monad m) => (a -> m a) -> Int -> a -> m a
iterateNM1 f n = concatM $ replicate n f

iterateNM2 :: (Monad m) => Int -> (a -> m a) -> a -> m a
iterateNM2 = flip iterateNM1

iterateNM3 :: Monad m => a -> Int -> (a -> m a) -> m a
iterateNM3 a i f = iterateNM1 f i a

-----------------------------------------------------------------
-----------------------------------------------------------------
                    {-     Fin      -}
-----------------------------------------------------------------
-----------------------------------------------------------------