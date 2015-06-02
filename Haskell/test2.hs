import System.Random

sort :: Ord a => [a] -> [a]
sort [] = []
sort xs
	| xs == sorth xs = xs
	| otherwise = sort $ sorth xs
sorth (x1:x2:xs)
	| x1 < x2 = x1:(sorth (x2:xs))
	| x2 <= x1 = x2:(sorth (x1:xs)) 
sorth [] = []
sorth (x:[]) = [x]
sorth (x:xs) = if x < (minimum xs) then x:sorth xs else (sorth (x:xs)) 



data Operator = X | MULT | DIV | SUM | SUB | Val Double  deriving Show

data Node a = Nil | Node {left :: Node a, value :: a, right :: Node a} deriving Show

data Anniversary = Birthday String Int Int Int       -- name, year, month, day
                 | Wedding String String Int Int Int -- spouse name 1, spouse name 2, year, month, day	
                 deriving Show


testTree1 = Node (Node Nil (Val 10) Nil) MULT (Node (Node Nil (X) Nil) SUM (Node Nil (Val 3) Nil))

testTree2 = Node (Node Nil (Val 10) Nil) MULT (Node (Node (Node Nil (X) Nil) SUB (Node Nil (Val 1) Nil)) MULT (Node (Node Nil (X) Nil) SUB (Node Nil (Val 1) Nil)))



evalTree :: Node Operator -> Double -> Double
evalTree Nil x = 0
evalTree (Node l v r) x = case v of
						Val vel -> vel
						X -> x
						MULT -> (*) (evalTree l x) (evalTree r x)
						DIV -> (/) (evalTree l x) (evalTree r x)
						SUM -> (+) (evalTree l x) (evalTree r x)
						SUB -> (-) (evalTree l x) (evalTree r x)

getRandomSubBranch :: Node Operator -> StdGen -> Node Operator
getRandomSubBranch c@(Node Nil v Nil) gen = c
getRandomSubBranch c@(Node l _ r) gen =	let (_,g) = next gen
											;n :: Integer; n = head $ randomRs (1,10) gen
										in case n of _
													| n < 5 -> getRandomSubBranch l g
													| n == 5 -> c
													| n > 5 -> getRandomSubBranch r g


mutateRandomSubBranch :: Node Operator -> StdGen -> Node Operator
mutateRandomSubBranch c@(Node Nil v Nil) gen = c
mutateRandomSubBranch c@(Node l v r) gen =	let (_,g) = next gen
											;n :: Integer; n = head $ randomRs (1,3) gen
										in case n of _
													| n < 5 -> mutateRandomSubBranch l g
													| n == 5 -> c
													| n > 5 -> mutateRandomSubBranch r g


fitnessFun :: Node Operator -> Double
fitnessFun n = let 	range = [0,1..10]
					;xs = map (evalTree n) range
					;fun x = 10*(x-1)^2
					;ys = map fun range
					;limit x 	| x < 0 = 0
								| x > 100 = 100
								| otherwise = x
				in limit $ 1 / (meanSquaredError xs ys) 

meanSquaredError ::  [Double] -> [Double] -> Double
meanSquaredError xs ys = let n = fromIntegral $ length xs in
	(foldr (+) 0 $ map (\(x,y) -> (x-y)^2) $ zip xs ys) / n
