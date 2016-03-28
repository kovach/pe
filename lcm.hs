import Prelude hiding (gcd)
import Data.List (foldl')
import Data.Map (Map, insertWith, empty, toList)

-- 0 "obviously correct" --
solve0 n = head $ filter ok [1..]
  where
    ok m = all (\k -> m `mod` k == 0) $ [1..n]

-- 1 --
factor 1 = []
factor n = let f = go 2 n in f : factor (n `div` f) where
    go k n = if mod n k == 0 then k else go (k+1) n

count k = length . filter (== k)

solve1 limit = product
  [ f ^ (maximum $ map (count f) fs)
    | f <- [2..limit]]
  where
    fs = map factor [1..limit]

-- 2 (also uses factor) more efficient version of 1 --
factor2 = rle . factor
  where
    rle [] = []
    rle (x : xs) = go 1 x xs
    go c x (y:ys) | y == x = go (c+1) x ys
    go c x ys = (x,c) : rle ys

step :: Map Integer Integer -> Integer -> Map Integer Integer
step m k = foldl' update m (factor2 k)
  where
    update m (k, v) = insertWith (\new old -> if new > old then new else old) k v m

solve2 n = product $ map (uncurry (^)) $ toList $ foldl' step empty [2..n]

-- 3 correct solution --
gcd x 0 = x
gcd a b = gcd b (a `mod` b)
solve3 n = foldl' step 1 [1..n]
  where
    -- fold reducer
    step n k = n * (k `div` (gcd n k))

-- 4 maximal standard library abuse --
solve4 n = foldr lcm 1 [1..n] -- foldr is faster than foldl' because lcm is asymmetric

-- a test --
-- doesn't check minimality
verify f k = all (== 0) [n `mod` k | k <- [1..k]]
  where
    n = f k

-- Call each one
-- leave out solve0 because slow
main = mapM_ check
  [ solve1
  , solve2
  , solve3
  , solve4
  -- , solve0
  ] where check f = print $ f 80
