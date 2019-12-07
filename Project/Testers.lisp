(load "Byte.lisp")

;;TESTING BASIC FUNCTIONS-------------------------------

;;(print (getSubList '(0 1 2 3 4 5 6 7 8 9) 3 4))
;;(print (readChar "Unesi jedan karakter"))
;;(print (indexOf '2 '(1 2 3)))
;;(print (evenIndexp '1 '(1 2 3)))
;;(print (div 13 2))
;;(print (memberList '(1 2) '(3 5 6 (1 2))))
;;(print (join '(1 2 (4 5)) '(3 2 (4 5))))
;;(print (countEl '(1 2) '(2 3 2 5 (1 2))))
;;(print (insertElement '(10) '(0 1 2 3 4 5 6) '1))
;;(print (getFirstN 3 '(0 1 2 3 4)))
;;(print (getLastN '7 '(1 2 3 4 5)))
;;(print (findIndexOf '2 '(0 1 2 3 2 4 5 6 2) '0))
;;(print (returnIfGreaterOrEqual '3 '(0 1 2 3 4 5 6 7)))
;;-----------------------------------------------------

;;TESTING BYTE GAME FUNCTIONS--------------------------

;;;;Moves

;;(setq FinalStacks (list BLACK WHITE BLACK))
;;(print (checkFinalState))

;;(setq myMove (create-move '(B 2) '(C 3) 0))
;;(setq myMove (create-move '() '() 0))
;;(readMove myMove)
;;(print myMove)

(setq myMove (create-move '(B 1) '(C 2) 0))
;;(print (moveTilesCheckp myMove 8))

;;(print (validTilesForMove '(B 1) (nearestTile '(B 1) (tilesOfValidStacks InitialState8x8 '(X) '())) 8))

;;(print (altitudeCheck '(B 1) '(C 5) '0 InitialState8x8))
;;(print (calculateResaultStack '(B 2) '(C 5) '0 InitialState8x8))
;;(print (calculateNewStartingTileStack '(B 2) '0 InitialState8x8))
;;(print (insertNewTileStack (calculateResaultStack '(B 2) '(C 5) '0 InitialState8x8) '(C 5) InitialState8x8))
;;(print (alterState myMove InitialState8x8))

;;States

(setq myState (create-state 8 InitialState8x8))
;(print myState)
;;(setq copyState (copyState myState))
;;(print copyState)

;;(print (formatEmptyStack '(-)))
;;(drawFirstRow)
;;(drawRow (cadr InitialState8x8) t)
;;(drawRow (cadr InitialState8x8) nill)
;;(print (getSingleRowList (cadar InitialState8x8) 1))
;;(drawSingleRow (getSingleRowList (cadar InitialState8x8) 1))
;;(print (getTilePrintList 9 (caadar InitialState8x8)))
;;(setq myState (create-initial-state 8))

;;(print (tileIndex 'D '0 PossibleRows '8))
;;(print (tileQuadrant '(B 4) '(D 4)))
;;(print (sameMainDiagonal '(B 4) '(C 5) 8))
;;(print (sameSideDiagonalp '(B 4) '(C 5) 8))
;;(print (checkTilep (tileIndex 'D '1 PossibleRows '8) 8))
;;(print (tileOutOfBoundsp '(B 8) 8))

;;(print (valueOfTile '(F 2) InitialState8x8))
;;(print (checkMergingPosibility '(O O O X X X) '(O O O O X)))
;;(print (tilesOfValidStacks InitialState8x8 '(X) '()))
;;(print (numOfMoves '(A 0) '(D 5)))
;;(print (nearestTile '(B 1) (tilesOfValidStacks InitialState8x8 '(X) '())))
;;(print (availableTileForGivenTile '(B 1) '(A 0) 8))

;;(print (moveExistsp myMove myState))
