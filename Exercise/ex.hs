f a [] = [a]
f a (x:xs) = a : f (a+x) xs

nth :: Integer -> [Integer] -> Integer
nth 0 (x:_) = x
nth n (_:xs) = nth (n-1) xs