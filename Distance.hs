module Distance  
( euclidDistance,
  hammingDistance   
) where  

euclidDistance :: Floating c => [c] -> [c] -> c
euclidDistance a b = sqrt (sum (zipWith (\a b -> (a - b)**2) a b))

hammingDistance :: Floating c => [c] -> [c] -> c
hammingDistance a b = sum $ zipWith (\x y -> abs (x - y)) a b