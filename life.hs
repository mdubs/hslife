import Data.List
data Point = Point Integer Integer deriving (Show, Eq)
type Boundary = (Integer,Integer,Integer,Integer)

tick :: [Point] -> [Point]
tick world = [ cell | cell <- cells_involved_in_this_tick, will_live cell world]
    where cells_involved_in_this_tick = world `union` (flatten (map neighbours world))

is_neighbour :: Point -> Point -> Bool
is_neighbour (Point x1 y1) (Point x2 y2)
    | is_same_point     = False
    | dx<=1 && dy<=1    = True
    | otherwise         = False
    where   dx = abs (x1-x2)
            dy = abs (y1-y2)
            is_same_point = x1 == x2 && y1 == y2

count_neighbours :: Point -> [Point] -> Integer
count_neighbours _ [] = 0
count_neighbours pt (x:xs) = 
    let single_neighbour = if is_neighbour pt x then 1 else 0
    in  single_neighbour + count_neighbours pt xs

will_live :: Point -> [Point] -> Bool
will_live pt pts
    | (not is_pt_alive) && number_of_neighbours == 3 = True
    | (not is_pt_alive) = False
    | number_of_neighbours < 2 = False
    | number_of_neighbours > 3 = False
    | otherwise = True
    where   number_of_neighbours = count_neighbours pt pts
            is_pt_alive = pt `elem` pts

neighbours :: Point -> [Point]
neighbours (Point x y) = [(Point (x-1) (y-1)), (Point x (y-1)), (Point (x+1) (y-1)),
                          (Point (x-1) y),                  (Point (x+1) y),
                          (Point (x-1) (y+1)), (Point x (y+1)), (Point (x+1) (y+1))]

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ (flatten xs) 

----------------------------------- Game Running and Iterations ------------------------------------------
main :: IO()
main = do
    boardInput <- getContents
    runGame (parseLines 0 (lines boardInput)) 10

runGame :: [Point] -> Integer -> IO()
runGame pts numIterations = runGameStep (maxCoords pts) pts numIterations

runGameStep :: Boundary -> [Point] -> Integer -> IO()
runGameStep _ _ 0 = putStrLn "THE END"
runGameStep currBoundary pts remaining =
    do
          putStrLn (unlines (board (min_x,min_y,max_x,max_y) pts))
          runGameStep (min_x,min_y,max_x,max_y) (tick pts) (remaining-1)

    where (new_min_x,new_min_y,new_max_x,new_max_y) = maxCoords pts
          (curr_min_x,curr_min_y,curr_max_x,curr_max_y) = currBoundary
          max_x = max new_max_x curr_max_x
          min_x = min new_min_x curr_min_x
          max_y = max new_max_y curr_max_y
          min_y = min new_min_y curr_min_y

----------------------------------- Input Code ------------------------------------------

parseLines :: Integer -> [String] -> [Point]
parseLines _ [] = []
parseLines lineNo (line:lines) = (parseLine (Point lineNo 0) line) ++ (parseLines (lineNo+1) lines)

parseLine :: Point -> String -> [Point]
parseLine _ [] = []
parseLine (Point x y) (char:chars)
    | char == '*'   = (Point x y) : parseLine nextPt chars
    | otherwise     = parseLine nextPt chars
    where
        nextPt = (Point x (y+1))

----------------------------------- Printing Code ------------------------------------------
board :: Boundary -> [Point] -> [[Char]]
board boundary pts = map (line pts y_range) x_range
    where (min_x, min_y, max_x, max_y) = boundary
          x_range = [min_x..max_x]
          y_range = [min_y..max_y]

line :: [Point] -> [Integer] -> Integer ->  [Char]
line pts y_points x_point = map (pointChar pts x_point) y_points

pointChar :: [Point] -> Integer -> Integer -> Char
pointChar alivepts x y
    | is_pt_alive = '*'
    | otherwise   = '-'
    where
         point = (Point x y)   
         is_pt_alive = point `elem` alivepts

maxCoords :: [Point] -> Boundary
maxCoords [] = (0,0,0,0)
maxCoords cells = (min_x, min_y, max_x, max_y)
    where max_x = maximum [ x | (Point x y) <- cells ]
          min_x = minimum [ x | (Point x y) <- cells ]
          max_y = maximum [ y | (Point x y) <- cells ]
          min_y = minimum [ y | (Point x y) <- cells ]

----------------------------------- Examples ------------------------------------------
slider :: [Point]
slider = [(Point 0 1), (Point 1 2), (Point 2 0), (Point 2 1), (Point 2 2)]

beacon :: [Point]
beacon = [(Point 0 0), (Point 0 1), (Point 1 0), (Point 1 1), (Point 2 2), (Point 2 3), (Point 3 2), (Point 3 3)]

blinker :: [Point]
blinker = [(Point 0 0), (Point 0 1), (Point 0 2)]

blinkerText :: [Char]
blinkerText = "*--\n*--\n*--"

----------------------------------- Testing Code ------------------------------------------
assert_equal :: Eq a => Show a => a -> a -> IO()
assert_equal x y
    | x == y = putStrLn "."
    | otherwise = putStrLn ("Expected " ++ show x ++ ", got " ++ show y)

mainTest :: IO ()
mainTest = do  
        assert_equal True (is_neighbour (Point 1 1) (Point 1 2))
        assert_equal False (is_neighbour (Point 1 1) (Point 1 1))
        assert_equal False (is_neighbour (Point 321 1) (Point 1 1))
        assert_equal 0 (count_neighbours (Point 0 0) [(Point 3 4)])
        assert_equal 1 (count_neighbours (Point 0 0) [(Point 1 1)])
        assert_equal False (will_live (Point 0 0) [])
        assert_equal False (will_live (Point 0 0) [(Point 1 1)])
        assert_equal True (will_live (Point 0 0) [(Point 0 0), (Point 1 0), (Point 1 1)])
        assert_equal True (will_live (Point 0 0) [(Point 0 1), (Point 1 0), (Point 1 1)])
        assert_equal False (will_live (Point 0 0) [(Point 1 0), (Point 1 1)])

