{-

Rush Hour Solver
Geoffrey McCord
Trey Nohren

-}

{-

function:  testforcars
purpose:   given a string representing a row, return an array of positions where cars are found
algorithm: recursively check each position in string for two characters that match, with exceptions
notes:     assumed no four character wide vehicles
           checking for truck and skipping ahead 2 covers the two cases where "CC" has a neighboring C

-}
testforcarsh :: String -> Int -> [Int]
testforcarsh row currentPos
    | currentPos > 4        = []

    -- skip ahead 1 and don't test '-' == '-'
    | leftchar == '-'       = testforcarsh row (currentPos + 1)

    -- this must be a truck, so skip ahead
    | leftchar == nextchar  = testforcarsh row (currentPos + 2)

    -- if not a truck, and two are same, then we have a car
    | leftchar == rightchar = currentPos:(testforcarsh row (currentPos + 1))

    -- continue to next position
    | otherwise             = (testforcarsh row (currentPos + 1))

    where leftchar = head (drop currentPos row)
          rightchar = head (drop (currentPos + 1) row)
          -- prevchar = if currentPos == 0 then '_' else head (drop (currentPos - 1) row)
          nextchar = if currentPos >= 4 then '-' else head (drop (currentPos + 2) row)

-- function to actually be called, sets up recursive function
testforcars :: String -> [Int]
testforcars row = if (length row /= 6) then [] else testforcarsh row 0



{-

function:  testfortruck
purpose:   given a string representing a row, return the position as Integer
algorithm: recurse by tail()-ing the string, keeping track of the last two
              chars. when they all match, return currentPos
notes:     assumed two trucks cannot be in the same row
           6 is returned (out of bounds) when no truck is detected

-}

testfortruckh :: String -> Char -> Char -> Int -> Int
testfortruckh row lastchar1 lastchar2 currentPos
    | null row          = 6
    | (head row == lastchar1) && (lastchar1 == lastchar2) && (head row /= '-')
                        = currentPos - 2
    | otherwise         = testfortruckh (tail row) (head row) (lastchar1) (currentPos + 1)

-- function to actually be called, sets up chars for recursion
testfortruck :: String -> Int
testfortruck row = testfortruckh row '-' '-' 0



{-

functions: makeRightMove, makeLeftMove
purpose:   when given a string representing the row and a vehicle's location and size,
              return the changed string for the type of move
algorithm: substring manipulation
notes:

-}
makeRightMove row startpos vehicletype
    = (take startpos row) ++ "-" ++ (take vehicletype (drop startpos row)) ++ (drop (startpos + vehicletype + 1) row)

makeLeftMove row startpos vehicletype
    = (take (startpos - 1) row) ++ (take vehicletype (drop startpos row)) ++ "-" ++ (drop (startpos + vehicletype) row)



{-

function:  makeMoves
purpose:   when given the string and the vehicle's position and length, return an array of strings of possible moves
algorithm: test for empty spaces, then combine strings representing moves into array
notes:     && operators are short-circuited, so head() cannot throw exception when vehicle's pos is at an end
           right move is "first" for XX car's benefit
-}
makeMoves row startpos vehicletype
    | rightmovepossible && not leftmovepossible     = [makeRightMove row startpos vehicletype]
    | leftmovepossible && not rightmovepossible     = [makeLeftMove row startpos vehicletype]

    | leftmovepossible && rightmovepossible         = [makeRightMove row startpos vehicletype,
                                                       makeLeftMove row startpos vehicletype]

    | otherwise                                     = []
        -- since && is short-circuited, head never can throw exception
        where leftmovepossible = (not (startpos <= 0)) && head (drop (startpos - 1) row) == '-'
              rightmovepossible = (not (startpos >= 6 - vehicletype)) && head (drop (startpos + vehicletype) row) == '-'



{-

function:  movesAvailableTruck
purpose:   combine the function that detects a truck and the function that makes moves for a truck,
              and return the array of strings
algorithm: give the move function the number returned from testfortruck()
notes:     this is where we test for the out-of-bounds 6 from testfortruck()

-}
movesAvailableTruck :: String -> [String]
movesAvailableTruck row
    | postruck < 6               = (makeMoves row postruck 3)
    | otherwise                  = []
         where postruck = testfortruck row



{-

function:  movesAvailableCars
purpose:   combine the function that detects cars and the function that makes moves for cars,
             and return the array of strings
algorithm: recursively run through array of detected car positions and give MakeMoves each position

-}
movesAvailableCarsh row ps
    | null ps = []
    | otherwise = (makeMoves row (head ps) 2) ++ (movesAvailableCarsh row (tail ps))

movesAvailableCars row = movesAvailableCarsh row (testforcars row)



{-

function:  allmovesAvailable
purpose:   combines the array of moves for trucks and the array of moves for cars
algorithm: append

-}
allmovesAvailable row = (movesAvailableTruck row) ++ (movesAvailableCars row)



{-

function:  movesExcept
purpose:   remove moves that return to a state previously visited
algorithm: use elem to detect if the move already in path, then remove it if so

-}
movesExceptF thelist exceptlist outlist
    | null thelist          = outlist
    | elem (head thelist) exceptlist = (movesExceptF (tail thelist) exceptlist outlist)
    | otherwise                     = (movesExceptF (tail thelist) exceptlist ((head thelist):outlist))

movesExcept thelist exceptlist = movesExceptF thelist exceptlist []



{-

function:  replaceHelper [helper to movesAvailableArray]
purpose:   given a list of moves, the current board, and the row to make the move on,
              return an array of boards to be traversed
algorithm: recurse through listOfMoves to generate list of boards,
              with each element containing a board with the indicated row changed

-}
replaceHelper :: [String] -> [String] -> Int -> [[String]]
replaceHelper listOfMoves currentBoard rowToChange
    | null listOfMoves                      = []
    | otherwise
        = ((take rowToChange currentBoard) ++ [(head listOfMoves)] ++ (drop (rowToChange + 1) currentBoard)) : (replaceHelper (tail listOfMoves) currentBoard rowToChange)



{-

function:  movesAvailableArray
purpose:   recurse through current board's rows to find moves for each row
algorithm: for each row, give helper function the row and list of moves for that row and combine results to make a list of boards
notes:     hard-coded to exit for a row size of six

-}
movesAvailableArray currentBoard rowToChange
    | rowToChange >= 6
        = []
    | (not (null x))
        = (replaceHelper x currentBoard rowToChange)++(movesAvailableArray currentBoard (rowToChange + 1))
    | otherwise
        = (movesAvailableArray currentBoard (rowToChange + 1))
        where x = (allmovesAvailable (head (drop rowToChange currentBoard)))



{-

function:  allMovesArray
purpose:   generate a complete list of boards resulting from every possible move when taken
algorithm: use the previous function to obtain boards representing moves
notes:     this is a helper function to combine "transposed" board (to make up/down moves) with "regular" board

-}
allMovesArray currentBoard
    = (movesAvailableArray currentBoard 0) ++ (map transpose (movesAvailableArray (transpose currentBoard) 0))

-- transpose code from Haskell library (sometimes not available?)
transpose               :: [[a]] -> [[a]]
transpose []             = []
transpose ([]   : xss)   = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])



{-

function:  isWin
purpose:   determine if the board is in a winning state
algorithm: check for the XX car being in\close to a winning position and
              return Integer representing "closeness"

-}
isWin currentBoard
  | (drop 4 theRow) == "XX"   = 0
  | (drop 3 theRow) == "XX-"  = 1
  | (drop 2 theRow) == "XX--" = 2
  | otherwise                 = 3
    where theRow = (head (drop 2 currentBoard))


{-

functions:  moveX_Board, moveX_Row
purpose:   force winning moves for faster execution time
algorithm: substring manipulation on boards detected to be close to win

-}
moveX1Board currentBoard = (take 2 currentBoard) ++ [(moveX1Row (head (drop 2 currentBoard)))] ++ (drop 3 currentBoard)
moveX1Row currentRow = (take 3 currentRow) ++ "-XX"

moveX2Board currentBoard = [moveX1Board firstMoveBoard] ++ [firstMoveBoard]
  where firstMoveBoard = (take 2 currentBoard) ++ [(moveX2Row (head (drop 2 currentBoard)))] ++ (drop 3 currentBoard)
moveX2Row currentRow = (take 2 currentRow) ++ "-XX-"



{-

function:  statesearch
purpose:   main function to traverse each possible move until isWin detects a win
algorithm: cons the moves onto path, use the path to prevent looping, return the list of boards left to explore
notes:     there is a following function to print result in a more readable fashion
-}
statesearch initialstate = statesearch1 [initialstate] []
statesearch1 unexplored path
   | null unexplored                 = []
   | (isWin (head unexplored)) == 0  = (head unexplored):path
   | (isWin (head unexplored)) == 1  = (moveX1Board (head unexplored)):[head unexplored] ++ path
   | (isWin (head unexplored)) == 2  = (moveX2Board (head unexplored)) ++ [head unexplored] ++ path
   | (not (null result))             = result
   | otherwise                       = statesearch1 (tail unexplored) path
     where result = statesearch1
                       (movesExcept (allMovesArray (head unexplored)) path)
                       ((head unexplored):path)



{-

function:  printresult
purpose:   print out list of boards to readable text
algorithm: recurse through boards, recurse through rows on new lines
notes:     generally too long to see in console
use:       printresult (statesearch ["------", "--C---","XXC---","------","------","------"])

-}
printboard board = do
    print (head board);
    if (null t) then print "," else printboard t;
    where t = tail board

printresult path = do
    printboard (head path);
    if (null t) then return null else printresult t;
    where t = tail path
