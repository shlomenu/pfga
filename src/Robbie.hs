import System.Random(randomRs)

mkSqGrid :: Int -> Int -> Array Int (UArray Int Word8)    
mkSqGrid seed dim = runSTArray $ do
    newArray(0, dim + 1) $ runSTUArray $ do 
        newArray(0, dim + 1) 0
    




