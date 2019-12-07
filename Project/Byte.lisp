;;External files used for the game
(load "BasicFunctions.lisp")

;;Constants
(defconstant EMPTY '-)
(defconstant WHITE 'O)
(defconstant BLACK 'X)
(defconstant INCREMENT '1)
(defconstant DECREMENT '-1)

;;Storing all posible InitialStates in variable

(setf InitialState8X8 '(
    (A ((-) (-) (-) (-)))
    (B ((X) (X) (X) (X))) 
    (C ((O) (O) (O) (O))) 
    (D ((X) (X) (X) (X))) 
    (E ((O) (O) (O) (O))) 
    (F ((X) (X) (X) (X))) 
    (G ((O) (O) (O) (O))) 
    (H ((-) (-) (-) (-)))))


(setf InitialState10X10 '(
    (A ((-) (-) (-) (-) (-)))
    (B ((X) (X) (X) (X) (X))) 
    (C ((O) (O) (O) (O) (O))) 
    (D ((X) (X) (X) (X) (X))) 
    (E ((O) (O) (O) (O) (O))) 
    (F ((X) (X) (X) (X) (X))) 
    (G ((O) (O) (O) (O) (O))) 
    (H ((X) (X) (X) (X) (X))) 
    (I ((O) (O) (O) (O) (O))) 
    (J ((-) (-) (-) (-) (-)))))

;;Saving list of possible rows in variable. List is used as helper list for faster search
(setf PossibleRows '(A B C D E F G H I J))

;;Storing stacks that are size of 8 and not on board anymore.
(setf FinalStacks '())

;;Checking if game is over and returning winner if it is, null if it's not
(defun checkFinalState ()
    (cond ((not (equal (length FinalStacks) 3)) '())
          (t(if (> (countEl BLACK FinalStacks) 1) BLACK WHITE))))

;;-------------------------------------------
;;Move structure
(defstruct (move 
    ;Options
    (:constructor create-move (previousTile nextTile height))
    (:print-object print-move))
    ;Atributes
    previousTile 
    nextTile
    height)

;;Option function for printing move on standard outstream
(defun print-move (moveToPrint outputStream)
  (format outputStream "~A<-~A" (move-nextTile moveToPrint) (move-previousTile moveToPrint)))

;;Reads move from standard input. Resault of reading is put in argument newMove that is instance od Move
(defun readMove (newMove) 
    (cond ((equalp (move-previousTile newMove) '())
            (prog1 
                (format t "Starting Tile~%") 
                (setf (move-previousTile newMove) (append (move-previousTile newMove) (readChar "Row: ")))
                (setf (move-previousTile newMove) (append (move-previousTile newMove) (readChar "Column: ")))
                (setf (move-height newMove) (readChar "Height: "))
                (readMove newMove)))
            ((equalp (move-nextTile newMove) '())
            (prog1 
                (format t "Targeted Tile~%") 
                (setf (move-nextTile newMove) (append (move-nextTile newMove) (readChar "Row: ")))
                (setf (move-nextTile newMove) (append (move-nextTile newMove) (readChar "Column: ")))
                (readMove newMove)))))

;;Checks if both tiles of move are valid.
(defun moveTilesCheckp (moveToCheck dimension)
    (if (and 
            (checkTilep (tileIndex  (car (move-previousTile moveToCheck)) 
                                    (cadr (move-previousTile moveToCheck)) 
                                    PossibleRows dimension) 
                        dimension)
            (checkTilep (tileIndex  (car (move-nextTile moveToCheck)) 
                                    (cadr (move-nextTile moveToCheck)) 
                                    PossibleRows dimension) 
                        dimension)) t '()))

;;Returnes all of valid tiles that player can move to in one move
(defun validTilesForMove (startingTile nearestTiles dimension) 
    (cond ((null nearestTiles) '())
         (t(join (list(availableTileForGivenTile startingTile (car nearestTiles) dimension))
                 (validTilesForMove startingTile (cdr nearestTiles) dimension)))))

;;Check altitude rule for two tiles
(defun altitudeCheck (startingTile targetedTile startingAltitude currentGameBoard) 
    (let ((heightOfStartingStack (length (valueOfTile startingTile currentGameBoard)))
          (heightOfTargetetStack (length (valueOfTile targetedTile currentGameBoard))))
          (cond ((> (+ heightOfStartingStack heightOfTargetetStack) 8) '())
                ((> startingAltitude heightOfTargetetStack) '())
                (t t))))

;;Returnes the resault targeted stack after move
(defun calculateResaultStack (startingTile targetedTile startingAltitude currentGameBoard)
    (let ((resaultStack (append (getFirstN (- (length (formatEmptyStack (valueOfTile startingTile currentGameBoard))) startingAltitude)
                                           (valueOfTile startingTile currentGameBoard))
                                (formatEmptyStack (valueOfTile targetedTile currentGameBoard)))))
         (if (equal (length resaultStack) 8) 
             (progn (setf FinalStacks (cons (car resaultStack) FinalStacks)) '())
             resaultStack)))

;;Returnes the resault of starting tile after the move
(defun calculateNewStartingTileStack (startingTile startingAltitude currentGameBoard)
    (let ((startingTileStack (valueOfTile startingTile currentGameBoard)))
         (cond ((>= startingAltitude (length startingTileStack)) startingTileStack)
               (t(getLastN startingAltitude startingTileStack)))))

;;Alters one Row of the game board after the move
(defun insertNewTileStack (newTileStack tileToAlter currentGameBoard)
    (cond ((null currentGameBoard) '())
          ((equal (car tileToAlter) (caar currentGameBoard))
            (cons (list (car tileToAlter) (insertElement newTileStack (cadar currentGameBoard) (div (cadr tileToAlter) 2)))
                  (cdr currentGameBoard)))
          (t(cons (car currentGameBoard) (insertNewTileStack newTileStack tileToAlter (cdr currentGameBoard))))))

;;Returnes new game board when move is performed
(defun alterState (moveToPerforme gameBoardToAlter)
    (insertNewTileStack (calculateNewStartingTileStack (move-previousTile moveToPerforme)
                                                       (move-height moveToPerforme)
                                                       gameBoardToAlter)
                        (move-previousTile moveToPerforme)
                        (insertNewTileStack (calculateResaultStack (move-previousTile moveToPerforme)
                                                                           (move-nextTile moveToPerforme)
                                                                           (move-height moveToPerforme)
                                                                           gameBoardToAlter)
                                                  (move-nextTile moveToPerforme) gameBoardToAlter)))

;;Checks if move exists
(defun moveExistsp (moveToCheck stateOfGame)
    (let* ((nextTileStack (valueOfTile (move-nextTile moveToCheck) (state-boardValues stateOfGame)))
           (previousTileStack (valueOfTile (move-previousTile moveToCheck) (state-boardValues stateOfGame))))
    (and (moveTilesCheckp moveToCheck (state-dimension stateOfGame))
        (tileOutOfBoundsp (move-previousTile moveToCheck) (state-dimension stateOfGame))
        (tileOutOfBoundsp (move-nextTile moveToCheck) (state-dimension stateOfGame))
        (altitudeCheck (move-previousTile moveToCheck) (move-nextTile moveToCheck) (move-height moveToCheck) (state-boardValues stateOfGame))
        (memberList (move-nextTile moveToCheck) (validTilesForMove
                                                 (move-previousTile moveToCheck)
                                                 (nearestTile (move-previousTile moveToCheck)
                                                              (tilesOfValidStacks (state-boardValues stateOfGame)
                                                                              previousTileStack
                                                                              '())) (state-dimension stateOfGame))))))


;;
(defun playMove (moveToPlay stateOfGame) 
    (alterState moveToPlay (copyState stateOfGame)))

;;
(defun getAllPossibleStates (currentState possibleMoves)
    (cond ((null possibleMoves) '())
          (t(cons (list (car possibleMoves) (alterState (car possibleMoves) currentState)) 
                  (getAllPossibleStates currentState (cdr possibleMoves))))))

;;Get all possible moves to performe starting from the given tile
(defun allposibleMovesFromGivenTile (curentTile validTilesForMove)
    (cond ((null validTilesForMove) '())
          (t(cons (create)))))
;;-------------------------------------------

;;State structure. Represents the state of game that player can find himself in.
(defstruct (state
    ;Options
    (:constructor create-state (dimension boardValues))
    (:print-object print-board))
    ;Atributes
    dimension
    boardValues)

;;Copies our state and returnes new
(defun copyState (stateToCopy) 
(create-state (state-dimension stateToCopy) (state-boardValues stateToCopy)))

;;Option function for printing State on standard outstream
(defun print-board (stateToPrint outputStream)
(prog1 (drawFirstRow)
    (do ((indexer 0 (+ indexer 1))) 
        ((= indexer (state-dimension stateToPrint)) indexer)
        (if (= (mod indexer 2) 0)
            (drawRow (nth indexer (state-boardValues stateToPrint)) t )
            (drawRow (nth indexer (state-boardValues stateToPrint)) '() )))))

;;Returnes nill instad of '(-)
(defun formatEmptyStack (listToFormat) (if (equalp listToFormat (list EMPTY)) '() listToFormat))
;;Helper function for printing first row of state representation
(defun drawFirstRow () 
    (format t "     ~{~a~^    ~}" '(1 2 3 4 5 6 7 8)))

;;Helper function for printing one Board row
(defun drawRow (boardRow parity) 
    (if (equalp parity t) 
        (prog1 
            (prog1 (format t "~%   ") (drawSingleRow (getSingleRowList (cadr boardRow) 1)))
            (prog1 (format t "~%~a  " (car boardRow)) (drawSingleRow (getSingleRowList (cadr boardRow) 2)))
            (prog1 (format t "~%   ") (drawSingleRow (getSingleRowList (cadr boardRow) 3))))
        (prog1 
            (prog1 (format t "~%        ") (drawSingleRow (getSingleRowList (cadr boardRow) 1)))
            (prog1 (format t "~%~a       " (car boardRow)) (drawSingleRow (getSingleRowList (cadr boardRow) 2)))
            (prog1 (format t "~%        ") (drawSingleRow (getSingleRowList (cadr boardRow) 3))))
        ))

;;Helper function that makes list for printing out of given values of a row
(defun getSingleRowList (stateRowLists singleRowNumber)
    (cond ((null stateRowLists) '())
        (t(cons (getSubList (getTilePrintList 9 (car stateRowLists)) (* (- singleRowNumber 1) 3) 3)
                (getSingleRowList (cdr stateRowLists) singleRowNumber)))))

;;Printing single row using the list made in getSingleRowList function
(defun drawSingleRow (singleRowList)
    (cond ((null singleRowList) (format t ""))
        (t(prog1 (format t "~{~a~^ ~}     " (car singleRowList)) (drawSingleRow (cdr singleRowList))))))


;;This funcion returnes list that represents the printing stack from one tile in our board
(defun getTilePrintList (tileSize tileList) 
    (cond ((or (equalp tileSize 0) (null tileList)) '())
        ((> (- tileSize (length tileList)) 0) (cons '- (getTilePrintList (- tileSize 1) tileList)) )
        (t(cons (car tileList) (getTilePrintList (- tileSize 1) (cdr tileList))))))

;;Helper function to create initial state of given dimensions
(defun create-initial-state (dimension) 
 (cond ((equalp dimension 8) (create-state dimension InitialState8X8))
        ((equalp dimension 10) (create-state dimension InitialState10X10))
        (t(format t "Invalid dimension for creating game"))))

;;Returnes the index value of a element in our state. Input is list (row column).
(defun tileIndex (row column listOfRows dimension) 
    (cond ((null listOfRows) '())
        ((equalp row (car listOfRows)) column)
        (t(+ dimension (tileIndex row column (cdr listOfRows) dimension)))))

;;Returnes the quadrant of given Tile
(defun tileQuadrant (centerTile targetedTile) 
    (let ((targetRow (indexOf (car targetedTile) PossibleRows))
          (centerRow (indexOf (car centerTile) PossibleRows))
          (targetColumn (cadr targetedTile))
          (centerColumn  (cadr centerTile)))
        (cond ((and (<= targetRow centerRow) (> targetColumn centerColumn)) '1)
              ((and (< targetRow centerRow) (<= targetColumn centerColumn)) '2)
              ((and (>= targetRow centerRow) (< targetColumn centerColumn)) '3)
              ((and (> targetRow centerRow) (>= targetColumn centerColumn)) '4)
              (t '1))))

;;Predicate for determinating whether the first tile and second tile are on the same diagonal
(defun sameMainDiagonal (firstTile secondTile dimension) 
(if (= (mod (tileIndex (car firstTile) (cadr firstTile) PossibleRows dimension) (+ dimension INCREMENT)) 
        (mod (tileIndex (car secondTile) (cadr secondTile) PossibleRows dimension) (+ dimension INCREMENT))) t '()))

;;Predicate for determinating whether the first tile and second tile are on the same side diagonal
(defun sameSideDiagonalp (firstTile secondTile dimension)
(if (= (mod (tileIndex (car firstTile) (cadr firstTile) PossibleRows dimension) (+ dimension DECREMENT)) 
        (mod (tileIndex (car secondTile) (cadr secondTile) PossibleRows dimension) (+ dimension DECREMENT))) t '()))
;;Checks if tile is valid for move. Argument must be index of tile that we get using tileIndex function.
;;Only Black tiles on the board are valid.
(defun checkTilep (tileIndex dimension) 
    (cond ((and (evenp (mod tileIndex dimension)) (evenp (div tileIndex dimension))) t)
        ((and (oddp (mod tileIndex dimension)) (oddp (div tileIndex dimension))) t)
        (t '())))

;;Checks if tile is out of bounds 
(defun tileOutOfBoundsp (tileToCheck dimension) 
    (if (or (not (member (car tileToCheck) (getFirstN dimension PossibleRows)))
            (> (cadr tileToCheck) dimension)
            (< (cadr tileToCheck) 0)) '() t))
;;Returnes the stack on the given tile. Tile is represented as '(row collum)
(defun valueOfTile (tile currentGameBoard) 
    (cond ((null currentGameBoard) (print "Not valid tile for this state"))
        ((equalp (car tile) (caar currentGameBoard)) (nth  (div (cadr tile) 2) (cadar currentGameBoard)))
        (t (valueOfTile tile (cdr currentGameBoard)))))

;;Returnes the diagonal tile with given directions
(defun diagonalTile (startingTile rowDirection columnDirection) 
    (list (nth (+ (indexOf (car startingTile) PossibleRows) rowDirection) PossibleRows) (+ (cadr startingTile) columnDirection)))

;Returnes all tiles that have given simbol on the top
(defun tilesOfValidStacks (currentGameBoard startingStackValue currentTile) 
    (cond ((null currentGameBoard) '())
          ((member (caar currentGameBoard) PossibleRows)
                (append (tilesOfValidStacks (cadar currentGameBoard)
                                            startingStackValue 
                                            (list (caar currentGameBoard) (mod (indexOf (caar currentGameBoard) PossibleRows) 2)))
                        (tilesOfValidStacks (cdr currentGameBoard) startingStackValue '())))
          (t(if (checkMergingPosibility startingStackValue (car currentGameBoard))
                (cons currentTile (tilesOfValidStacks (cdr currentGameBoard)
                                                      startingStackValue
                                                      (list (car currentTile) (+ 2 (cadr currentTile)))))
                (tilesOfValidStacks (cdr currentGameBoard)
                                                      startingStackValue
                                                      (list (car currentTile) (+ 2 (cadr currentTile))))))))

;;Returnes t if mearging of two stacks is possible
(defun checkMergingPosibility (startingStack targetedStack)
    (let* ((startingHeight (length startingStack))
           (targetedHeight (length targetedStack))
           (symbolIndexes (findIndexOf (car (last startingStack)) startingStack 0))
           (validAltitudeIndexes (returnIfGreaterOrEqual (- startingHeight targetedHeight) symbolIndexes)))
           (cond ((and (< (- startingHeight targetedHeight) 0) (<= (+ startingHeight targetedHeight) 8)) t)
                 ((and (not (null validAltitudeIndexes)) (<= (+ targetedHeight (+ INCREMENT (car validAltitudeIndexes))) 8)) t)
                 (t '()))))

;Returnes number of moves needed to get from starting tile to targeted tile
(defun numOfMoves (startingTile targetedTile)
    (cond ((or (null startingTile) (null targetedTile)) (print "Invalid starting or targeted tile!"))
        (t (max (abs (- (indexOf (car startingTile) PossibleRows) (indexOf (car targetedTile) PossibleRows)))
                (abs (- (cadr targetedTile) (cadr startingTile)))))))

;;Returnes one or more tile from the list that is the nearest to our tile 
(defun nearestTile (startingTile listOfTiles) 
    (cond ((null startingTile) (print "Starting tile is empty!"))
          ((= 1 (length listOfTiles)) listOfTiles)
          (t(let* ((bestTiles (nearestTile startingTile (cdr listOfTiles)))
                  (currentBest (numOfMoves startingTile (car bestTiles)))
                  (possibleBest (numOfMoves startingTile (car listOfTiles))))
                (cond ((= possibleBest 0) bestTiles)
                      ((= currentBest possibleBest) (cons (car listOfTiles) bestTiles))
                      ((> currentBest possibleBest) (list(car listOfTiles)))
                      ((< currentBest possibleBest) bestTiles))))))

;;Returnes tiles that are possible to play with given starting tile and one nearest tile
(defun availableTileForGivenTile (startingTile singleNearestTile dimension)
    (let ((quadrantOfTarget (tileQuadrant startingTile singleNearestTile)))
         (cond ((= quadrantOfTarget 1) (if (sameSideDiagonalp startingTile singleNearestTile dimension)
                                           (diagonalTile startingTile DECREMENT INCREMENT)
                                            (list 
                                                (diagonalTile startingTile DECREMENT INCREMENT)
                                                (diagonalTile startingTile INCREMENT INCREMENT))))
               ((= quadrantOfTarget 2) (if (sameMainDiagonal startingTile singleNearestTile dimension)
                                            (diagonalTile startingTile DECREMENT DECREMENT)
                                            (list 
                                                (diagonalTile startingTile DECREMENT DECREMENT)
                                                (diagonalTile startingTile DECREMENT INCREMENT))))
               ((= quadrantOfTarget 3) (if (sameSideDiagonalp startingTile singleNearestTile dimension)
                                            (diagonalTile startingTile INCREMENT DECREMENT)
                                            (list 
                                                (diagonalTile startingTile INCREMENT DECREMENT)
                                                (diagonalTile startingTile DECREMENT DECREMENT))))
               ((= quadrantOfTarget 4) (if (sameMainDiagonal startingTile singleNearestTile dimension)
                                            (diagonalTile startingTile INCREMENT INCREMENT)
                                            (list 
                                                (diagonalTile startingTile INCREMENT INCREMENT)
                                                (diagonalTile startingTile DECREMENT INCREMENT)))))))
;;-------------------------------------------
;;Structure for a player
(defstruct 
    (player
    (:constructor create-player (&optional symbol)))
    symbol)

;;Structure for a game
(defstruct 
    (byteGame
    (:conc-name game)
    (:constructor create-game
        (player1 
         player2
         &key
         state
         (modified-board (copy-state state)))))
    board
    modified-board
    (modifier n-modifier)
    player1
    player2
    (end? nil)
    (winner nil))