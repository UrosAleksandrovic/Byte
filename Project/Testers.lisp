(load "Byte.lisp")

(setq myBoard '(
    (A ((-) (-) (-) (O)))
    (B ((-) (-) (-) (-))) 
    (C ((-) (X O X) (-) (-))) 
    (D ((-) (-) (-) (-))) 
    (E ((-) (-) (X) (-))) 
    (F ((O) (-) (-) (-))) 
    (G ((-) (-) (-) (-))) 
    (H ((-) (-) (-) (-)))))

;;TESTING BASIC FUNCTIONS-------------------------------

;;(print (getSubList '(0 1 2 3 4 5 6 7 8 9) 3 4))
;;(print (readChar "Unesi jedan karakter"))
;;(print (indexOf '1 '(1 2 3)))
;;(print (evenIndexp '2 '(1 2 3)))
;;(print (div 13 2))
;;(print (memberList '(1 2) '(3 5 6 (1 2))))
;;(print (join '(1 2 (4 5)) '(3 2 (4 5))))
;;(print (countEl '(1 2) '(2 3 2 5 (1 2))))
;;(print (insertElement '(10) '(0 1 2 3 4 5 6) '2))
;;(print (getFirstN 3 '(0 1 2 3 4)))
;;(print (getLastN '2 '(1 2 3 4 5)))
;;(print (findIndexOf '2 '(0 1 2 3 2 4 5 6 2) '0))
;;(print (returnIfGreaterOrEqual '3 '(0 1 2 3 4 5 6 7)))
;;-----------------------------------------------------

;;TESTING BYTE GAME FUNCTIONS--------------------------

;;;;Moves

;;(setq finalState (create-state '8 myBoard BLACK))
;;(setf (state-finalStacks finalState) '(X O X))
;;(print (checkFinalState finalState))

;;(setq myMove (create-move '(B 2) '(C 3) 0))
;;(setq myMove (create-move '() '() 0))
;;(print myMove)
;;(print (readMove))

;;(setq myMove (create-move '(D 4) '(C 3) 0))
;;(print (moveTilesCheckp myMove 8))

;;(print (validTilesForMove '(B 2) (nearestTile '(B 2) (tilesOfValidStacks myBoard '(X) '())) 8))

;;(print (altitudeCheck '(B 1) '(C 5) '0 myBoard))

;;(setq testerState (create-state '8 InitialState8x8 BLACK))
;;(print (calculateResaultStack '(B 2) '(C 5) '0 testerState))
;;(print (calculateNewStartingTileStack '(B 2) '0 testerState))
;;(print (insertNewTileStack (calculateResaultStack '(B 2) '(C 5) '0 testerState) '(C 5) InitialState8x8))
;;(print (alterState myMove testerState))

;;States

;;(setq myState (create-state 8 myBoard BLACK))
;;(print myState)
;;(setq copyState (copyState myState))
;;(print copyState)

;;(print (formatEmptyStack '(-)))
;;(drawFirstRow)
;;(drawRow (cadr InitialState8x8) t)
;;(drawRow (cadr InitialState8x8) '())
;;(print (getSingleRowList (cadar InitialState8x8) 1))
;;(drawSingleRow (getSingleRowList (cadar InitialState8x8) 1))
;;(print (getTilePrintList 9 (caadar InitialState8x8)))
;;(setq myState (create-initial-state 8))

;;(print (tileIndex 'D '1 PossibleRows '8))
;;(print (tileQuadrant '(B 4) '(C 1)))
;;(print (sameMainDiagonal '(B 4) '(C 5) 8))
;;(print (sameSideDiagonalp '(B 4) '(C 3) 8))
;;(print (checkTilep (tileIndex 'D '2 PossibleRows '8) 8))
;;(print (tileOutOfBoundsp '(B 8) 8))
;;(print (diagonalTile '(A 1) INCREMENT DECREMENT 8))

;;(print (valueOfTile '(F 2) InitialState8x8))
;;(print (checkMergingPosibility '(C) '(X 0)))
;;(print (tilesOfValidStacks myBoard '(O) '()))
;;(print (numOfMoves '(F 2) '(F 2)))
;;(print (nearestTile '(F 2) (tilesOfValidStacks myBoard '(O) '())))
;;(print (availableTileForGivenTile '(B 4) '(D 2) 8))

(setq myState (create-state 8 myBoard WHITE))
(setq myMove (create-move '(A 7) '(B 6) 0))
;;(print (moveExistsp myMove myState))

;;(print (gettingAllStartingTilesForPlayer myBoard WHITE '()))
;;(print (allPossibleMovesFromGivenTile myBoard 8 '(F 2)))
;;(print (allPossibleMovesFromGivenTile myBoard 8 '(A 7)))
(print myState)
(print (allPossibleMovesForPlayerOnMove myState))

;;(print myState)
;;(print (getAllPossibleStates myState))
;;(print (state-FinalStacks myState))

;;(setq potez2 (readMove))
;;(setq state1 (create-initial-state 8))


;;(print (createStateGraph (getAllPossibleStates myState)))
;;(print (findMaxState (createStateGraph (getAllPossibleStates myState))))
;;(print (findMinState (createStateGraph (getAllPossibleStates myState))))

;;----------------------------------
;;(setq testerState (create-initial-state 8))
;;(print (alphabeta (list myState '0) 1 (list myState -∞) (list myState +∞) t))