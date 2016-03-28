module Sample where
import System.Random

type Rand = IO

bit :: Rand Int
bit = randomRIO (0,1)

biased n = (== 1) <$> randomRIO (1, n)

-- number of flips needed to get a success
geom :: Rand Int
geom = go 0 where
  go n = do
    b <- bit
    if b == 0 then return n else go (n+1)

sample :: Int -> Rand a -> Rand [a]
sample n d = mapM (\_ -> d) [1..n]

-- Test value, Sample size, Distribution
binom :: Eq a => a -> Int -> Rand a -> Rand Int
binom v n d = length . filter (== v) <$> sample n d

binomial :: Int -> Rand Bool -> Rand Int
binomial = binom True

data Tree = T [Tree] deriving (Eq, Show)

--pp (T []) = "*"
pp (T ts) = "(" ++ concatMap pp ts ++ ")"

tree :: Rand Tree
tree = do
  n <- geom
  T <$> sample n tree

edges :: Tree -> Int
edges (T ts) = length ts + sum (map edges ts)

-- naive conditioning using a predicate
condition :: (a -> Bool) -> Rand a -> Rand a
condition f d = do
  val <- d
  if f val then return val else condition f d

treeN n = condition ((== n) . edges) tree

average n d = do
  set <- sample n d
  return $ fromIntegral (sum set) / fromIntegral n
