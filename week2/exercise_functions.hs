halve1 xs = splitAt (length xs `div` 2) xs
halve2 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs
--drops the element after the first half and returns the remaining
halve3 xs = (take n xs, drop (n + 1) xs) 
  where n = length xs `div` 2
halve4 xs = splitAt (div (length xs) 2) xs
halve5 xs = (take n xs, drop n xs)
  where n = length xs `div` 2