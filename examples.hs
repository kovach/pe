fold :: (x -> a -> x) -> x -> [a] -> x
fold reducer initial [ ] = initial
fold reducer accumulator (x : rest) =
  let new_accumulator = reducer accumulator x
  in fold reducer new_accumulator rest

sum_numbers values = fold (+) 0 values
product' values    = fold (*) 1 values
mean xs = sum_numbers xs / (fromIntegral $ length xs)

-- max :: Ord a => a -> a -> a
maxOfList :: Ord a => [a] -> a
maxOfList (x : xs) = fold max x xs
minOfList (x : xs) = fold min x xs

