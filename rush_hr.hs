-- Olga Ivanova 999084690 Farjad Siddiqui 913375961 ECS 140B Spring 2017
-- Rush Hour Project

rush_hour start = printAns (reverse (statesearch [start] []))

--------------------------------
-- Functions to print out answer
--------------------------------
printBoard a = do
   mapM_ putStrLn a
   putStrLn "\n"

printAns []  = putStrLn "[]"
printAns ans = mapM_ printBoard ans

---------------------------------------------------------
-- Transpose function, adapted (but not copied!!!) from:
-- https://codereview.stackexchange.com/questions/48451/implementing-transpose
-------------------------------------------------------------------------------
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose rows = (map head rows) : transpose (map tail rows)


-- For filter in statesearch
nElem myList myEl = notElem myEl myList

-----------------------------------------
-- Basic State Search, taken from slides
-----------------------------------------
statesearch :: [[String]] -> [[String]] -> [[String]]
statesearch [] _ = []
statesearch (x:xs) path    
   | drop 4 (x!!2) == "XX" = x:path
   | elem x path           = []
   | not (null newstates)  = newstates    
   | otherwise             = statesearch xs path    
   where newstates = statesearch (filter (nElem path) (generateNewStates x)) (x:path) 
   -- this last bit should filter out loops (hopefully)

generateNewStates :: [String] -> [[String]] 
generateNewStates currState = concat  [generateVertMoves currState, generateHorizMoves currState]

---------------------------------
-- Horizontal and Vertical Moves
---------------------------------
generateVertMoves :: [String] -> [[String]]
generateVertMoves currState = map transpose (concat [generateMovesLeft  [] cols [], generateMovesRight [] cols []])
                              where cols = transpose currState


generateHorizMoves :: [String] -> [[String]]
generateHorizMoves currState = concat [generateMovesLeft [] rows [], generateMovesRight [] rows []]
                               where rows = map (\(a) -> a) currState
                               -- Had to convert tuple to list here, but not in VertMoves?? Not sure why

--------------------------------------------------
-- generatesMovesLeft/Right:
-- already converted rows/columns to same format,
-- so moves can be generated the same way
--------------------------------------------------
generateMovesLeft :: [String] -> [String] -> [[String]] ->[[String]]
generateMovesLeft _ [] posMoves = posMoves
generateMovesLeft beforeRows (x:xs) posMoves
   | length (listVehs x) == 1 
     && not (null (moveLeft [] x 1)) = generateMovesLeft (beforeRows++[x]) xs ((moveVeh 1):posMoves)
   | length (listVehs x) == 2 
     && not (null (moveLeft [] x 1)) = generateMovesLeft (beforeRows++[x]) xs (concat [[moveVeh 1], [moveVeh 2], posMoves])
   | length (listVehs x) == 2 
     && not (null (moveLeft [] x 2)) = generateMovesLeft (beforeRows++[x]) xs ((moveVeh 2):posMoves)
   | otherwise                       = generateMovesLeft (beforeRows++[x]) xs posMoves
   where moveVeh n = concat [beforeRows, [moveLeft [] x n], xs]


generateMovesRight :: [String] -> [String] -> [[String]] ->[[String]]
generateMovesRight _ [] posMoves = posMoves
generateMovesRight beforeRows (x:xs) posMoves
   | length (listVehs x) == 1 
     && not (null (moveRight [] x 1)) = generateMovesRight (beforeRows++[x]) xs ((moveVeh 1):posMoves)
   | length (listVehs x) == 2
     && not (null (moveRight [] x 1)) = generateMovesRight (beforeRows++[x]) xs (concat [[moveVeh 1], [moveVeh 2], posMoves])
   | length (listVehs x) == 2 
     && not (null (moveRight [] x 2)) = generateMovesRight (beforeRows++[x]) xs ((moveVeh 2):posMoves)
   | otherwise                        = generateMovesRight (beforeRows++[x]) xs posMoves
   where moveVeh n = concat [beforeRows, [moveRight [] x n], xs]

------------------------------------------------------
-- moveLeft, moveRight:
-- takes a number as an argument
-- because there might be more than one car in a line
-- "moves" car down line, if possible
------------------------------------------------------
moveLeft :: String -> String -> Int -> String
moveLeft _ [] 1 = []
moveLeft beforeSpots (x:xs) 1
   | elem x currRow && null beforeSpots          = []
   | elem x currRow && (last beforeSpots) /= '-' = []
   | elem x currRow && (last beforeSpots) == '-' = concat [init beforeSpots, getVeh (x:xs) x, "-", end]
   | otherwise                                   = moveLeft (beforeSpots++[x]) xs 1
   where currRow = listVehs (x:xs)
         end     = drop ((length (getVeh (x:xs) x)) - 1) xs

moveLeft [] currSpots 2 = reverse (moveRight [] (reverse currSpots) 1)

moveRight :: String -> String -> Int -> String
moveRight _ [] 1 = []
moveRight beforeSpots (x:xs) 1
   | elem x currRow && null xs          = []
   | elem x currRow && (head xs) == '-' = concat [beginning, "-", getVeh (reverse (beforeSpots++[x])) x, tail xs]
   | otherwise                          = moveRight (beforeSpots++[x]) xs 1
   where currRow   = listVehs (beforeSpots++[x])
         beginning = take ((length beforeSpots) - (length (getVeh (reverse beforeSpots) x))) beforeSpots
   -- These string manipulation parts are not so great :(

moveRight [] currSpots 2 = reverse (moveLeft [] (reverse currSpots) 1)

-----------------------------------------------
-- getVeh:
-- returns string which represents a car/truck
-----------------------------------------------
getVeh :: String -> Char -> String
getVeh [] _ = []
getVeh (x:xs) currSpot
   | x == currSpot = currSpot:(getVeh xs currSpot)
   | otherwise     = []
 
----------------------------------------
-- listVehs:
-- returns list of vehicles in that row
----------------------------------------
listVehs :: String -> String
listVehs mystring = helpVehs mystring '-' []

helpVehs :: String -> Char -> String -> String
helpVehs [] _ acc = acc
helpVehs (x:xs) lastChar acc
   | (x == lastChar)
      && (not (lastChar == '-')) 
      && (not (elem lastChar acc)) = helpVehs xs x (x:acc)
   | otherwise                     = helpVehs xs x acc
