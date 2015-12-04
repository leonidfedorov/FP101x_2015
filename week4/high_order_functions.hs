all2 p xs = and (map p xs)

any3 p = or . map p
any4 p xs = not (all (\ x ->  not (p x)) xs)
any5 p = not . null . dropWhile (not . p)
any6 p xs = foldr (\ x acc -> (p x) || acc) False xs

takeWhile2 _ [] = []
takeWhile2 p (x : xs)
  | p x = x : takeWhile2 p xs
  | otherwise = []

dropWhile2 _ [] = []
dropWhile2 p (x : xs)
  | p x = dropWhile2 p xs
  | otherwise = x : xs

map2 f = foldl (\ xs x -> xs ++ [f x]) []

filter2 p = foldr (\ x xs -> if p x then x : xs else xs) []

dec2int = foldl (\ x y -> 10 * x + y) 0


compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

curry2 f = \ x y -> f (x, y)

uncurry2 f = \ (x, y) -> f x y

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

type Bit = Int
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop82 = unfold null (take 8) (drop 8)

map3 f = unfold null (f . head) tail

iterate2 f = unfold (const True) id f