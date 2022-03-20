import Prelude
import Data.Char ( ord, chr, isLower, isAsciiLower )

-- Pair up adjacent entries.
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- Check ordering.
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

-- Get indices of equivalent entries.
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

-- Generate Pythagorean triples.
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [x+1..n], z <- [y+1..n], x^2 + y^2 == z^2]

char2int :: Char -> Int
char2int c = ord c - ord 'a'

int2char :: Int -> Char
int2char n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2char ((char2int c + n) `mod` 26)
          | otherwise = c

count :: Eq a => a -> [a] -> Int
count x xs = length [x' | x' <- xs, x == x']

lowers :: String -> Int
lowers xs = count True (map isAsciiLower xs)

caesarEncode :: Int -> String -> String
caesarEncode n xs = [shift n x | x <- xs]

-- Frequency of letters in English
frequencyTable :: [Float]
frequencyTable = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) (lowers xs) | x <- ['a'..'z']]

-- Compute the chi-square.
chisqr :: [Float] -> [Float] -> Float 
chisqr os es = sum [((o-e)^2)/e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

caesarCrack :: String -> String 
caesarCrack xs = caesarEncode (-factor) xs
    where
        factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') frequencyTable | n <- [0..25]]
        table' = freqs xs