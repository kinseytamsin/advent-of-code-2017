import Data.Char
import Data.Monoid
import System.IO

foldSum :: (Foldable t, Num a) => t a -> a
foldSum = getSum . foldMap Sum

digits :: String -> [Int]
digits = map digitToInt

neighborDigits :: Int -> [Int] -> [(Int, Int)]
neighborDigits n = take <$> length <*> (zip <$> cycle <*> (drop n . cycle))

matchingDigitsSum :: Int -> [Int] -> Int
matchingDigitsSum n = foldSum
                      . map fst
                      . filter (uncurry (==))
                      . neighborDigits n

putStrLnShow :: Show a => a -> IO ()
putStrLnShow = putStrLn . show

main = do
    withFile "1.txt" ReadMode (\hdl -> do
        inputDigits <- fmap digits $ hGetLine hdl
        let inputMatchingDigitsSum = flip matchingDigitsSum inputDigits
        let putSum = putStrLnShow . inputMatchingDigitsSum
        putSum 1
        putSum $ (length inputDigits) `quot` 2)
