import Data.List
data Point = Point Integer Integer deriving (Show, Eq)

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

----------------------------------- Printing Code ------------------------------------------
runGame :: [Point] -> Integer -> IO()
runGame _ 0 = putStrLn "THE END"
runGame pts remaining = do
          printBoard pts
          runGame (tick pts) (remaining-1)

printBoard :: [Point] -> IO()
printBoard a = putStrLn (unlines (board a))
board :: [Point] -> [[Char]]
board a = map (cellLine a) (map (line [min_y..max_y]) [min_x..max_x])
    where  min_y = 0
           max_y = 10
           min_x = 0
           max_x = 10

cellLine :: [Point] -> [Point] -> [Char]
cellLine alivePts linePoints = map (cell alivePts) linePoints

cell :: [Point] -> Point -> Char
cell alivepts point
    | is_pt_alive = '*'
    | otherwise   = '-'
    where   is_pt_alive = point `elem` alivepts

line :: [Integer] -> Integer -> [Point]
line a b = map (point b) a

point :: Integer -> Integer -> Point
point a b = (Point a b) 

----------------------------------- Testing Code ------------------------------------------
assert_equal :: Eq a => Show a => a -> a -> IO()
assert_equal x y
    | x == y = putStrLn "."
    | otherwise = putStrLn ("Expected " ++ show x ++ ", got " ++ show y)

main :: IO ()
main = do  
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

