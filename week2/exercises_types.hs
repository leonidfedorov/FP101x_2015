second xs = head(tail xs)
swap (x, y) = (y, x)
pair x y = (x, y)
double x = x * 2
palindrome xs = reverse xs == xs
twice f x = f ( f x)
f xs = take 3 (reverse xs)
mult a b c d = a * b * c * d