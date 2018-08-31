import Data.Char
import Data.Monoid
import System.IO

neighborDigits :: Int -> [Int] -> [(Int, Int)]
neighborDigits n = take <$> length <*> (zip <$> cycle <*> (drop n . cycle))

matchingDigitsSum n = getSum . foldMap Sum
                      . map fst
                      . filter (uncurry (==))
                      . neighborDigits n

digits = map digitToInt

putStrLnShow = putStrLn . show

main = do
    withFile "1.txt" ReadMode (\hdl -> do
        inputDigits <- fmap digits $ hGetLine hdl
        let inputMatchingDigitsSum = flip matchingDigitsSum inputDigits
        let putSum = putStrLnShow . inputMatchingDigitsSum
        putSum 1
        putSum $ (length inputDigits) `quot` 2)
