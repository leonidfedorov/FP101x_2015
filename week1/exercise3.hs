init5 xs = reverse (tail (reverse xs)) -- doesn't work on empty lists like original init
init6 xs = take (length xs - 1) (tail xs) -- works on empty lists unlike original init