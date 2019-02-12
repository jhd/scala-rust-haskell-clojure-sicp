import Debug.Trace
data Tree = Branch Tree Tree [String] Int | Leaf deriving (Show)

mergeBranches :: Tree -> Tree -> Tree
mergeBranches Leaf Leaf = Leaf
mergeBranches l@(Branch ll lr lsym lw) r@(Branch rl rr rsym rw) = Branch l r (lsym ++ rsym) (lw + rw)

insertOrdered :: Tree -> [Tree] -> [Tree]
insertOrdered Leaf xs = xs
insertOrdered b [] = [b]
insertOrdered b@(Branch _ _ _ w) rest@(x@(Branch _ _ _ ew):xs) 
    | ew < w = x : insertOrdered b xs
    | otherwise = b : rest

successiveMerge :: [Tree] -> Tree
successiveMerge [] = Leaf
successiveMerge [x] = x
successiveMerge (x:y:xs) = successiveMerge $ insertOrdered (mergeBranches x y) xs

encodeSym :: String -> Tree -> [Int]
encodeSym _ Leaf = []
encodeSym _ (Branch Leaf _ _ _) = []
encodeSym sym (Branch l@(Branch _ _ lsyms _ ) r _ _ )
    | elem sym lsyms = 0 : encodeSym sym l
    | otherwise = 1 : encodeSym sym r 


generateHuffmanTree :: [(String, Int)] -> Tree
generateHuffmanTree freqList = successiveMerge $ map (\(sym, weight) -> Branch Leaf Leaf [sym] weight) freqList

encode :: [String] -> Tree -> [Int]
encode [] _ = []
encode (sym:syms) tree = encoded ++ encode syms tree
    where
        encoded = traceShow (sym, (encodeSym sym tree)) $ encodeSym sym tree

decode :: [Int] -> Tree -> [String]
decode xs tree = decode' xs tree
                    where
                        decode' [] (Branch _ _ syms _) = [head syms]
                        decode' xs (Branch Leaf _ syms _) = head syms : decode' xs tree
                        decode' (0:xs) t@(Branch l _ _ _) = decode' xs l
                        decode' (1:xs) t@(Branch _ r _ _) = decode' xs r

main = do 
    putStrLn $ show $ tree
    putStrLn $ show $ encoded
    putStrLn $ show $ decoded
    where
        tree = generateHuffmanTree $ reverse [("NA", 16), ("YIP", 9), ("SHA", 3), ("A", 2), 
                              ("GET", 2), ("JOB", 2), ("BOOM", 1), ("WAH", 1)]
        encoded = encode ["NA", "YIP", "SHA", "JOB"] tree
        decoded = decode encoded tree
