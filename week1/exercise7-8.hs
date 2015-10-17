qsort1 [] = []
qsort1 (x:xs) = qsort1 larger ++ [x] ++ qsort1 smaller
 where smaller = [a | a <- xs, a <= x]
       larger = [b | b <- xs, b > x]

qsort5 [] = []
qsort5 (x:xs) = qsort5 larger ++ [x] ++ qsort5 smaller
 where larger = [a | a <- xs, a > x || a == x]
       smaller = [b | b <- xs, b < x]

qsort7 [] = []
qsort7 (x : xs) 
 = reverse
      (reverse (qsort7 smaller) ++ [x] ++ reverse (qsort7 larger))
 where smaller = [a | a <- xs, a <= x]
       larger = [b | b <- xs, b > x]

-- this returns a set of sorted elements, instead of a sorted list
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
 where
  smaller = [ a | a <- xs, a < x]
  larger = [ b | b <- xs, b > x]
