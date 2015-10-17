product2 [] = 1
product2 [x] = x
product2 (x: y: xs) = x * product2 xs