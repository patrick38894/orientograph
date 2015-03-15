import Data.List
import Data.Maybe
import Control.Monad

--
--
-- B <- set of all darts
-- theta(b) <- other dart on same edge as b     eg: theta(a->b) = b->a
-- sigma(b) <- succ(b) in the cyclic ordering
--
--


type Graph = [Vertex]
type Vertex = [Int] --list of darts as indices to Graph
type Dart = (Int, Int) --(index of vertex, index of edge)
data Permutation = Perm [Int] --permutation


--functions used to generate permutations sigma and theta
theta :: Graph -> Dart -> Dart
theta g (v,e) = (e, fromJust $ elemIndex v $ g !! e)

sigma :: Graph -> Dart -> Dart
sigma g (v,e) = (v, (e + 1) `mod` (length $ g !! v))

--group class for easy construction of groups
class Group g where
	e :: g
	(*) :: g -> g -> g
	gmul :: g -> g -> g
	inv :: g -> g
	
--group instances

instance Group Permutation where --permuation group
	e = Perm [0,1..]
	gmul (Perm a) (Perm b) = Perm $ map (\x -> b !! x) a
	a * b = a `gmul` b
	inv (Perm a) = Perm $ invBubSort a 0
		where invBubSort a n = case elemIndex n a of Just x -> x:(invBubSort a $ n+1)
							     Nothing -> []
		-- ^ O(n2) is the least of our worries



--create a permutation from a list of mapping pairs
pzip :: [(Int, Int)] -> Permutation
pzip a = Perm $ map snd $ sortBy (\x y -> compare (fst x) (fst y)) a

--construct embedded rotation system from graph g

rotations :: Graph -> (Permutation, Permutation)
--construct set of all Darts (connected vertex, di-edges)
--generate permutations for the darts
rotations g = (pzip $ map (sigma g) b, pzip $ map (theta g) b)
	where b = darts g


--construct set of all darts from a digraph
darts :: Graph -> [Dart]
darts g = darts' g 0
	where darts' [] n = []
	      darts' (v:vs) n = zip (repeat n) v ++ (darts' vs $ n+1)


--to find the genus use Euler formula -> genus = 1 -1/2(|Z(sig)|-|Z(th)|+|Z(sig*th)|)
--where Z is the set of orbits of a permutation

orbit :: Permutation -> Int -> [Int] --note that this implementation will not iterate infinite sets
orbit (Perm p) i = let orbit' i s = if (p!!i) `elem` s
				    then []
				    else (p!!i):(orbit' (p!!i) $ (p!!i):s)
		   in orbit' i []


--find the genus of graph g
genus :: Graph -> Int
genus g = let b = darts g
	      sz a = length $ partitionWith (orbit a) [0.. length b - 1]
	      rot = rotations g
	      t = snd rot
	      s = fst rot
	      st = s `gmul` t
	  in 1 - (sz s - sz t + sz st) `quot` 2




----------------utility----------------
partitionWith :: (Eq a) => (a -> [a]) -> [a] -> [[a]]
partitionWith f [] = []
partitionWith f (x:xs) = (f x):(partitionWith f $ filter (\a -> not $ a `elem` f x) xs)




