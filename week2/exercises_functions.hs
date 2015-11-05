import Prelude hiding ((||)) --this helps to redefine the  (||) operator for exercise 2
import Prelude hiding((&&))

--EXERCISE 0
halve1 xs = splitAt (length xs `div` 2) xs

halve2 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs

--WRONG example below: drops the element after the first half and returns the remaining
halve3 xs = (take n xs, drop (n + 1) xs) 
  where n = length xs `div` 2

halve4 xs = splitAt (div (length xs) 2) xs

halve5 xs = (take n xs, drop n xs)
  where n = length xs `div` 2

--EXERCISE 1
safetail1 xs = if null xs then [] else tail xs

safetail2 [] = []
safetail2 (_ : xs) = xs

safetail3 xs
  | null xs = []
  | otherwise = tail xs

safetail4 [] = []
safetail4 xs = tail xs

safetail5
  = \ xs -> 
      case xs of 
      	[] -> []
      	(_ : xs) -> xs

--EXERCISE 2: alternative implementations of the logical or (||) operator with the same definition
False || False = False
_ || _ = True

False ||- b = b
True ||- _ = True

b ||-| c
  | b == c = b
  | otherwise = True

b ||-|- False = b
_ ||-|- True = True

b ||-|-| c
  | b == c = c
  | otherwise = True

False ||-|-|- False =  False
False ||-|-|- True = True
True ||-|-|- False = True
True ||-|-|- True = True

--EXERCISE 3: alternative implementations of the logical or (&&) operator with the same definition
True && True = True
_ && _ = False

a &&* b = if a then if b then True else False else False

a &&** b = if a then b else False

a &&*** b = if b then a else False 

--EXERCISE 4: defining multiplication of three numbers through lambda-currying
mult = \x -> ( \y -> ( \z -> x * y * z))

--EXERCISE 7
remove n xs = take n xs ++ drop (n+1) xs

--EXERCISE 8
funct :: Int -> [a] -> [a]
funct x xs = take (x + 1) xs ++ drop x xs