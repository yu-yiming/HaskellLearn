import Data.Char

type Bit = Int

-- Convert a binary list to its decimal equivalent.
bin2int :: [Bit] -> Int
bin2int bits = sum $ zipWith (*) bits (iterate (*2) 1)

-- Convert an integer to binary as list of characters.
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- Align to 8.
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- Convert an integer in the string form to binary as list of characters.
encode :: String -> [Bit]
encode = concatMap (make8 . int2bin . ord)

-- Chop the binary list by 8 each time.
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- Convert a binary list to its decimal equivalent as string.
decode :: [Bit] -> String 
decode = map (chr . bin2int) . chop8

-- Transmit a string through an implementation-defined channel, id by default.
transmit :: String -> String 
transmit = decode . channel . encode
    where channel = id


count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

-- Remove duplicates in a list.
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : filter (/=x) (rmdups xs)

-- Quick sort.
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort left ++ [x] ++ qsort right
    where
        left = [a | a <- xs, a <= x]
        right = [b | b <- xs, b > x]

voteResult :: Ord a => [a] -> [(Int, a)]
voteResult xs = qsort [(count x xs, x) | x <- rmdups xs]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/=[])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map $ filter (/=x)

rank :: Ord a => [[a]] -> [a]
rank = map snd . voteResult . map head

winner :: Ord a => [[a]] -> a
winner xs = case rank (rmempty xs) of
                [c] -> c
                (c : cs) -> winner (elim c xs)
                _ -> undefined

