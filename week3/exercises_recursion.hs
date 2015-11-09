m ^| 0 = 1
m ^| n = m * m ^| (n - 1)

m ^|| 0 = 1
m ^|| n =  m * (^||) m (n-1)

and2 :: [Bool] -> Bool
and2 [] = True
and2 (b : bs) = b && and2 bs

and3 :: [Bool] -> Bool
and3 [] = True
and3 (b : bs) = and3 bs && b

and4 :: [Bool] -> Bool
and4 (b : bs)
   | b = and4 bs
   | otherwise = False

and5 :: [Bool] -> Bool
and5 (b : bs)
   | b == False = False
   | otherwise = and5 bs

concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (xs:xss) = xs ++ concat2 xss

replicate2 :: Int -> a -> [a]
replicate2 0 _ = []
replicate2 n x = x : replicate2 (n - 1) x

--selecting n-th element fromt the list, work on non-empty listst only
(!!-) :: [a] -> Int -> a
(x : _) !!- 0 = x
(_ : xs) !!- n = xs !!- (n - 1)

elem2 :: Eq a => a -> [a] -> Bool
elem2 _ [] = False
elem2 x (y : ys)
   | x == y = True
   | otherwise = elem2 x ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  = if x <= y then x : merge xs (y : ys) else y : merge (x : xs) ys


halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort zs) (msort ys)
  where (ys, zs) = halve xs
