{-
Advent of Code 2018 - Day 23 - Part 2 solution
The main idea is to create one big cube that envelopes all octahedrons and then recursively split it (octatree-style like), until the point with most intersections is found.
The cubes are kept in a priority queue and are ordered by their score, which is the amount of octahedrons it intersects with.
That way, the best candidates are searched first. The search ends when the best cube in search queue has smaller score than the best, already found, point.
-}



import Data.List
import Data.Ord
import Data.Function
import Data.Maybe

{- http://hackage.haskell.org/package/pqueue-1.4.1.2/docs/Data-PQueue-Prio-Max.html -}
import qualified Data.PQueue.Prio.Max as PQueue

data Octahedron = Octahedron {
    octahedronCenter :: Coords,
    octahedronRadius :: Radius
    } deriving (Show, Read)

data Cube = Cube {
    cubeCenter :: Coords,
    cubeRadius :: Radius
} deriving (Show)

data Coords = Coords { 
    x :: Int,
    y :: Int,
    z :: Int
} deriving (Show, Read)

type Radius = Int

{- Ordering is by the cube's score, which is the amount of octahedrons it intersects with -}
type SearchQueue = PQueue.MaxPQueue Int Cube

{- Parsed line: "pos=<89743969,58072157,20035651>, r=62659871" -}
lineToOctahedron :: String -> Octahedron
lineToOctahedron line =
    let
        f ',' = ' '
        f c = c
        integers = map read $ words $ filter (`elem` "0123456789 -") $ map f line :: [Int]
        in Octahedron (Coords (integers !! 0) (integers !! 1) (integers !! 2)) (integers !! 3)

{- From the cube center, walk diagonally as far as possible (for 1 range we move one space in up to 3 dimensions) and then walk on one axis until the octahedron's center is reached (or not) -}
isCubeAndOctahedronIntersecting :: Cube -> Octahedron -> Bool
isCubeAndOctahedronIntersecting cube octahedron = let
    distanceAxis axis = abs $ (axis $ cubeCenter $ cube) - (axis $ octahedronCenter $ octahedron)
    distance = sort $ (map distanceAxis [x, y, z])
    remaining = if (head distance) >= (cubeRadius cube) 
        then (sum distance) - (3 * cubeRadius cube) -- walking diagonally
    else let
        walkedDiagonally = (cubeRadius cube) - (head distance)
        remainingDistance = [(distance !! 1) - walkedDiagonally, (distance !! 2)] 
        remainingCubeRadius = (cubeRadius cube) - walkedDiagonally
        in if (head remainingDistance > remainingCubeRadius)
            then (sum remainingDistance) - (2 * remainingCubeRadius)
    else 0 -- The octahedron is enveloped in the cube
            
    in octahedronRadius octahedron >= remaining

countIntersections :: Cube -> [Octahedron] -> Int
countIntersections cube octahedrons = length $ filter (==True) $ map (isCubeAndOctahedronIntersecting cube) octahedrons
{- return a static list, since using infinite list disallows the usage of tail -}
getPowerOfTwo :: [Int]
getPowerOfTwo = [0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608, 16777216, 33554432, 67108864, 134217728, 268435456, 536870912, 1073741824, 2147483648, 4294967296, 8589934592]
--getPowerOfTwo =  map (\x -> 2^x) [0..]

{- Return a Cube containing all octahedrons in the list on input -}
findBoundaryCube :: [Octahedron] -> Cube
findBoundaryCube list = let

    findCenter from to = (from + to) `div` 2
    findRadius from to = abs(from - (findCenter from to)) + (if odd(from + to) then 1 else 0) -- HACK to avoid off-by-one error when the divison in findCenter is rounded
    roundToPowerOfTwo int = head $ filter (\x -> x >= int) $ getPowerOfTwo

    {- Returns a tuple of center point on the axis and it's radius -}
    computeCoord axis = let 
        max = maximum $ map (\octahedron -> (axis $ octahedronCenter octahedron) + (octahedronRadius octahedron)) list
        min = minimum $ map (\octahedron -> (axis $ octahedronCenter octahedron) - (octahedronRadius octahedron)) list
        in ((findCenter max min), (findRadius max min))

    tuples = [computeCoord] <*> [x, y, z]
    createCoords [x, y, z] = (Coords x y z)
    radius = maximum $ map snd tuples
    in Cube (createCoords $ map fst tuples) (roundToPowerOfTwo radius)

{- Splits a cube into 8 smaller cubes. -}
splitCube :: Cube -> [Cube]
splitCube cube = let

    centerCoords = cubeCenter cube
    generatePairs x y = [x - y, x + y]
    generateTriples x = [x] ++ (generatePairs x 1)
    generateCoords center radius = [(Coords x' y' z') | x' <- generatePairs (x center) radius, y' <- generatePairs (y center) radius, z' <- generatePairs (z center) radius]
    
    -- step down to the previous cube length integer
    newRadius = last $ filter(\x -> x < cubeRadius cube) $ getPowerOfTwo

    in 
        if newRadius == 0 then [Cube (Coords x' y' z') newRadius | x' <- generateTriples $ x centerCoords, y' <- generateTriples $ y centerCoords, z' <- generateTriples $ z centerCoords]
        else [Cube center newRadius | center <- generateCoords (cubeCenter cube) newRadius]

distanceToBeginning :: Cube -> Int
distanceToBeginning cube = let coords = cubeCenter cube in  x coords + y coords + z coords -- in case of a tie, a cube closer to (0, 0, 0) is chosen

{- find a point with the largest amount of intersections with all octahedrons -}
findIntersectionCenter :: [Octahedron] -> SearchQueue -> (Int, Cube) -> Maybe Cube
findIntersectionCenter octahedrons queue (maxScore, maxCube) = let
    searchedCube = PQueue.getMax queue
    poppedQueue = PQueue.deleteMax queue
    in case searchedCube of
        Nothing -> Nothing -- the search failed, for whatever reason
        Just (score, cube) -> 
            if (score < maxScore) then Just maxCube -- found the best possible point, since it has bigger score than any other cube in the (priority!) queue
            else let 
                splittedCubes = splitCube cube
                evaluatedCubes =  map(\x -> (countIntersections x octahedrons, x)) splittedCubes -- assign a score to each cube
                in if (cubeRadius cube == 1) then let -- the splitted cubes have radius 0, so they're evaluated manually
                -- find the point with the biggest score that is closest to the center
                    mostIntersectionCount = fst $ maximumBy (comparing fst) evaluatedCubes
                    mostIntersectionsCube = head $ sortBy (\x y -> compare (distanceToBeginning $ snd x) (distanceToBeginning $ snd y)) $ filter (\x -> fst x == mostIntersectionCount) evaluatedCubes
                in if (fst mostIntersectionsCube > maxScore) then
                    findIntersectionCenter octahedrons poppedQueue (fst mostIntersectionsCube, snd mostIntersectionsCube)
                -- found point has the same score as the current best, but is closer to the center
                else if (fst mostIntersectionsCube == maxScore && (distanceToBeginning $ snd mostIntersectionsCube) < (distanceToBeginning maxCube)) then
                    findIntersectionCenter octahedrons poppedQueue (fst mostIntersectionsCube, snd mostIntersectionsCube)
                else findIntersectionCenter octahedrons poppedQueue (maxScore, maxCube)
            else let -- push the new cubes into the queue
                updatedQueue = foldl (\x y -> PQueue.insert (fst y) (snd y) x) poppedQueue evaluatedCubes
                in findIntersectionCenter octahedrons updatedQueue (maxScore, maxCube)

main = do
    log <- lines <$> readFile "input.txt" :: IO [String]
    let octahedrons = map lineToOctahedron log
    let boundary = findBoundaryCube octahedrons
    let queue = PQueue.singleton (length octahedrons) boundary
    let maxCube = findIntersectionCenter octahedrons queue (0, boundary)
    print $ maxCube
    print $ "Expected result is: 105191907"
    case maxCube of
        Nothing -> print $ "Search failed. This should not have happened."
        Just cube -> 
            print $ distanceToBeginning cube
            