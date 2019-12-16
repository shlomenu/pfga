module Main where

import Robbie
import Control.Monad(forM)

errString :: String
errString = "pop-size dim nstep ngen can-density log-freqency mutn-rate crossover-rate" ++ 
            "\n                  :: Int Int Int Int Float Int Float Float"

main :: IO ()
main = do
    pn <- getProgramName
    args <- getArgs   
    let (psize, dim, nStep, nGen, canDensity,
             logFreq, mutRate, crossRate) = parseArgs pn args
        let ev = evolve dim nStep logFreq canDensity mutRate crossRate
    gnms <- -- makeGenomes
    endGen <- iterateNM ev nGen (0, gnms)
    -- do final logging actions
    -- save the winning genome

parseArgs :: String -> [String] -> (Int,Int,Int,Int,Float,Int,Float,Float)
parseArgs pn (a:b:c:d:e:f:g:h:_) = fmap read (a,b,c,d,e,f,g,h)
parseArgs pn _ = "usage: " ++ pn ++ errString   

evolveS :: Int -> Int -> Int -> Float -> Float -> Float 
                  -> (Int, [Genome]) -> IO (Int, [Genome])
evolveS dim nstep lf dens mutr crossr (i, gnms) = do
    -- make simulators w/ genomes
    -- run them nsteps using iterateNM
    -- retrieve scores from them
    -- select best + produce next generation via mutn + cross
    -- log scores in some way
    -- `return` IO [Genome] of new genomes

evolveP :: Int -> Int -> Int -> Float -> Float -> Float
                  -> (Int, [Genome]) -> IO (Int, [Genome])
evolveP dim nstep lf dens mutr crossr (i, gnms) = do

iterateNM :: (Monad m) => (a -> m a) -> Int -> a -> m a
iterateNM f n = concatM $ replicate n f

runNGensS :: Int -> [Sim] -> IO [Sim]
runNGensS = iterateNM runGenS where
    runGenS = sequence . map act

evalGenS :: [Sim] -> 
    
