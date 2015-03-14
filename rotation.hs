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
type Perm = [Int] --permutation


--functions used to generate permutations sigma and theta
theta :: Graph -> Dart -> Dart
theta g (v,i) = (i, fromJust $ elemIndex v $ g !! i)

sigma :: Graph -> Dart -> Dart
sigma g (v,i) = (v, (i + 1) `mod` length $ g !! v)

--group class for easy construction of groups
class Group g where
	e :: g
	(*) :: g -> g -> g
	gmul :: g -> g -> g
	inv :: g -> g
	
--group instances

instance Group Perm where --permuation group
	e = [0,1..]
	gmul a b = map (\x -> b !! x) a
	a * b = a gmul b
	inv a = invBubSort a 0
		where invBubSort a n = case elemIndex n a of Just x -> x:(invBubSort a n+1)
							     Nothing -> []
		-- ^ O(n2) is the least of our worries

--construct embedded rotation system from graph g

rotations :: Graph -> (Perm, Perm)
--construct set of all Darts (connected vertex, di-edges)
--generate group of permutations for the darts
rotations g = (map sigma b, map theta b)
	where darts [] n = []
	      darts (v:vs) n = zip (repeat n) v ++ darts vs n+1
	      b = darts g 0


--to find the genus use Euler formula -> genus = 1 -1/2(|Z(sig)|-|Z(th)|+|Z(sig*th)|)
--where Z is the set of orbits of a permutation

orbits :: Perm -> Int -> [Int] --note that this implementation will not work for infinite sets
orbits p i = let orbits' i s = if (p!!i) `elem` s
			       then []
			       else (p!!i):(orbits (p!!i) $ (p!!i):s)
	     in orbits' i []


--find the genus of graph g
genus :: Graph -> Int
genus g = let sz a = length $ orbits a
	      rot = rotations g
	      t = snd rot
	      s = fst rot
	      st = nub $ liftM2 (gmul) s t
	  in 1 - (sz s - sz t + sz st) `quot` 2








