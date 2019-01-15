isEven n = n `mod` 2 == 0

sameParity [] = []
sameParity (x:xs)
    | isEven x  = x : filter isEven xs
    | otherwise = x : filter (\x -> not $ isEven x) xs

main = do putStrLn $ show $ sameParity [1, 2, 3, 4, 5, 6, 7]
          putStrLn $ show $ sameParity [2, 3, 4, 5, 6, 7]
