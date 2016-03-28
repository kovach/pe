import Prelude hiding (lookup)
import Data.List (maximumBy, foldl')
import Data.Function (on)
import Data.Map (insert, empty, lookup, toList, Map)

step n | n `mod` 2 == 0 = n `div` 2
step n = 3*n+1

-- 0 --
coll 1 = [1]
coll n = let n' = step n in n : coll n'

solve0 n = maximumBy (compare `on` snd) $ zip [1..n] (map (length . coll) [1..n])
avg n = (sum $ map (length . coll) [1..n]) `div` n

-- 1 --
type D = Map Integer Integer
col :: Integer -> D -> Integer -> (Integer, D)
col _ m 1 = (1, m)
col b m n | n < b , Just v <- lookup n m = (v, m)
col b m n =
  let (v, m') = col b m (step n)
      v' = 1+v
      m'' = if n < b then insert n v' m' else m'
  in (v', m'')

solve1 :: Integer -> (Integer, Integer)
solve1 n =
  let m = foldl' (\m k -> snd (col bound m k)) empty [1..n]
  in maximumBy (compare `on` snd) $ toList m
  where
    bound = n

-- test --
t n = (solve0 n, solve1 n)
