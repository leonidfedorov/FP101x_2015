last_alt1 xs  = drop (length xs - 1) xs --works on empty lists unlike original last
last_alt2 xs = head (drop (length xs - 1) xs) --doesn't work on empty lists just like original last
last_alt3 xs = xs !! (length xs - 1) --doesn't work on empty lists though just like original last, but the error is different
last_alt4 xs = head (reverse xs) --doesn't work on empty lists just like original last