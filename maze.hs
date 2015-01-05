data Wall = Wall Int Int deriving (Show)
data Maze = Maze Int Int [Wall] [Bool] deriving (Show)

getWalls :: Maze -> [Wall]
getWalls (Maze d n ws ss) = ws

setWalls :: Maze -> [Wall] -> Maze
setWalls (Maze d n ws ss) ws' = Maze d n ws' ss

pushWall :: Maze -> Wall -> Maze
pushWall (Maze d n ws ss) w = Maze d n (w:ws) ss

pullWall :: Maze -> Wall
pullWall (Maze d n ws ss) = head ws

initial :: Int -> Int -> Maze
initial d n = Maze d m w s
	where
		s = take (d ^ n) (True : repeat(False))
		m = 2 * n + 1
		w = w1 : w2 : []
			where
				w1 = Wall 1 1
				w2 = Wall m m

recur :: Maze -> Maze
recur m
	| length (getWalls m) > 0 = m
	| otherwise = m
		  

i = recur (initial 2 6)