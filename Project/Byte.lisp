;;External files used for the game
(load "BasicFunctions.lisp")

;;Constants
(defconstant EMPTY '-)
(defconstant WHITE 'O)
(defconstant BLACK 'X)

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

(setf PossibleRows '(A B C D E F G H I J))

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

;;State structure. Represents the state of game that player can find himself in.
(defstruct (state
    ;Options
    (:constructor create-state (dimension boardValues))
    (:print-object print-board))
    ;Atributes
    dimension
    boardValues)


;;Option function for printing State on standard outstream
;;-------------------------------------------
(defun print-board (stateToPrint outputStream)
(prog1 (drawFirstRow)
    (do ((indexer 0 (+ indexer 1))) 
        ((= indexer (state-dimension stateToPrint)) indexer)
        (if (= (mod indexer 2) 0)
            (drawRow (nth indexer (state-boardValues stateToPrint)) t )
            (drawRow (nth indexer (state-boardValues stateToPrint)) '() )))))

;;Helper function for printing first row of state representation
(defun drawFirstRow () 
    (format t "     ~{~a~^    ~}" '(1 2 3 4 5 6 7 8)))

;;Helper function for printing one State row
(defun drawRow (stateRow parity) 
    (if (equalp parity t) 
        (prog1 
            (prog1 (format t "~%   ") (drawSingleRow (getSingleRowList (cadr stateRow) 1)))
            (prog1 (format t "~%~a  " (car stateRow)) (drawSingleRow (getSingleRowList (cadr stateRow) 2)))
            (prog1 (format t "~%   ") (drawSingleRow (getSingleRowList (cadr stateRow) 3))))
        (prog1 
            (prog1 (format t "~%        ") (drawSingleRow (getSingleRowList (cadr stateRow) 1)))
            (prog1 (format t "~%~a       " (car stateRow)) (drawSingleRow (getSingleRowList (cadr stateRow) 2)))
            (prog1 (format t "~%        ") (drawSingleRow (getSingleRowList (cadr stateRow) 3))))
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
;;-------------------------------------------

;;Helper function to create initial state of given dimensions
(defun create-initial-state (dimension) 
 (cond ((equalp dimension 8) (create-state dimension InitialState8X8))
        ((equalp dimension 10) (create-state dimension InitialState10X10))
        (t(format t "Invalid dimension for creating game"))))

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


;;Checks if move exists
(defun moveExistsp (moveToCheck dimension)
    (let 
        ((previousTileIndex (tileIndex 
                                    (car (move-previousTile moveToCheck)) 
                                    (cadr (move-previousTile moveToCheck))  
                                    PossibleRows dimension)) 
        (nextTileIndex (tileIndex  
                                (car (move-nextTile moveToCheck)) 
                                (cadr (move-nextTile moveToCheck)) 
                                PossibleRows dimension)))
    (and (moveTilesCheckp moveToCheck dimension) (checkDiagonalyp previousTileIndex nextTileIndex dimension))))



;;Returnes the index value of a element in our state. Input is list (row column).
(defun tileIndex (row column listOfRows dimension) 
    (cond ((null listOfRows) '())
        ((equalp row (car listOfRows)) column)
        (t(+ dimension (tileIndex row column (cdr listOfRows) dimension)))))

;;Checks if tile is valid for move. Argument must be index of tile that we get using tileIndex function.
(defun checkTilep (tileIndex dimension) 
    (cond ((and (evenp (mod tileIndex dimension)) (evenp (div tileIndex dimension))) t)
        ((and (oddp (mod tileIndex dimension)) (oddp (div tileIndex dimension))) t)
        (t '())))

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

;;Checks if tiles are diagonaly one tile away
(defun checkDiagonalyp (previousTileIndex nextTileIndex dimension) 
    (cond ((= previousTileIndex (+ nextTileIndex(- dimension 1))) t) 
        ((= previousTileIndex (+ nextTileIndex (+ dimension 1))) t) 
        ((= previousTileIndex (- nextTileIndex (- dimension 1))) t) 
        ((= previousTileIndex (- nextTileIndex (+ dimension 1))) t)
        (t '())))

;;Playing the move section---------------------------
(defun valueOfTile (tile state) 
    (cond ((null state) (print "Not valid tile for this state"))
        ((equalp (car tile) (caar state)) (nth  (div (cadr tile) 2) (car (cdar state))))
        (t (valueOfTile tile (cdr state)))))


