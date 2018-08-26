import System.IO

neighborDigits :: Int -> [Int] -> [(Int, Int)]
neighborDigits neighborDistance xs = take (length xs)
    $ zip (cycle xs) (drop neighborDistance $ cycle xs)

matchingDigitsSum :: Int -> [Int] -> Int
matchingDigitsSum neighborDistance = foldl (+) 0
                                     . map fst
                                     . filter (\x -> fst x == snd x)
                                     . neighborDigits neighborDistance

charToString :: Char -> String
charToString c = [c]

digits :: String -> [Int]
digits x = map (read::String->Int) $ map charToString x

main = do
    hdl <- openFile "1.txt" ReadMode
    contents <- hGetLine hdl
    putStrLn $ show $ matchingDigitsSum 1 (digits contents)
    putStrLn $ show
        $ matchingDigitsSum
            ((length $ digits contents) `quot` 2)
            (digits contents)
    hClose hdl
