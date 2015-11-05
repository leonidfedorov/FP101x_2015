sum100 = sum [ x ^ 2 | x <- [1 .. 100]] -- sum of squares of all integers from 1 to 100

replicate n a = [a | _ <- [1 .. n]]

pyths n
  = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x^2 + y^2 == z^2]

factors n = [x | x <- [1 .. n], n `mod` x == 0]

prime n = factors n == [1, n]

slowprimes n = [x | x <- [2 .. n], prime x]

perfects n = [x | x <- [1 .. n], isPerfect x]
  where isPerfect num = sum (factors num) == num

pairs xs = zip xs (tail xs)

positions1 :: Eq a => a -> [a] -> [Int]
positions1 x xs  =
   [i | (x', i) <- zip xs [0 .. n], x == x']
   where n = length xs - 1

sorted :: Ord a => [a] -> Bool
sorted xs = 
	and [x <= y | (x, y) <- pairs xs]

find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions2 :: (Eq a) => a -> [a] -> [Int]
positions2 x xs = find x (zip xs [0 .. n])
  where n = length xs - 1

scalarproduct xs ys = sum [ x * y | (x, y) <- xs `zip` ys]

xs1 = 1 : [x + 1 | x <- xs1] -- infinite list starting at 1

riffle xs ys = concat [[x, y] | (x,y) <- xs `zip` ys]

divides1 x y = x `mod` y == 0

divides2 x y
	| x `mod` y == 0 = True
	| otherwise = False

divisors x = [d | d <- [1 .. x], x `divides1` d]