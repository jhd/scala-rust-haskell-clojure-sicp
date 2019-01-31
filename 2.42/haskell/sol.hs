queens :: Int -> [[Int]]
queens boardSize = filter isSafe $ queenCols boardSize
           where queenCols 0 = [[]]
                 queenCols k = filter isSafe $ concatMap (\pos -> map (\queen -> queen : pos) $ [1..boardSize]) $ queenCols(k-1)
                 -- Slower version (\queen -> map (\pos -> queen : pos) $ queenCols (k-1)) [1..boardSize]

isSafe :: [Int] -> Bool
isSafe []     = True
isSafe [q]    = True
isSafe (q:qs) = isSafe' q (q-1) (q+1) qs
                where isSafe' nq _   _   [] = True
                      isSafe' nq bot top (q:qs)
                            | nq  == q  = False
                            | bot == q  = False
                            | top == q  = False
                            | otherwise = isSafe' nq (bot-1) (top+1) qs 

main = do putStrLn $ show $ queens 4
          putStrLn $ show $ queens 8
