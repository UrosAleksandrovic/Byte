(load "Byte.lisp")

;;;;;;;;;;;;;;TESTER FOR PRINTING SINGLE ROW
;(print (getSingleRowList '((1 2 3 4 5 6 7 8) (2 4) (5 5 5 5) (9 8 7 6 5 4 3) (2) (1) (5) (7)) '2) )
;(format t "~%")
;(drawSingleRow (getSingleRowList '((1 2 3 4 5 6 7 8) (2 4) (5 5 5 5) (9 8 7 6 5 4 3) (2) (1) (5) (7)) '1) )
;(format t "~%")
;(drawSingleRow (getSingleRowList '((1 2 3 4 5 6 7 8) (2 4) (5 5 5 5) (9 8 7 6 5 4 3) (2) (1) (5) (7)) '2) )
;(format t "~%")
;;(drawRow '(A ((1 2 3 4 5 6 7 8) (2 4) (5 5 5 5) (9 8 7 6 5 4 3)))) 
;;---------------------------------------------------------------------------------

;;;;;;;;;;TESTER FOR PRINTING STATE
;;(print (create-initial-state 8))
;--------------------------------------------------------------


;;;;;;;;;;;;;;TESTER FOR READING MOVE
;(setq myMove (create-move '(F 1) '(E 1)))
;(readMove myMove)
;(print myMove)
;-------------------------------------------

;;;;TESTER FOR EVALUATION OF MOVE
;(print (checkTilep (tileIndex 'C '6 PossibleRows) 8))
;(print (checkTilep (tileIndex 'H '5 PossibleRows) 8))
;(print (checkTilep (tileIndex 'F '4 PossibleRows) 8))

;(print (moveExistsp myMove 8))

(print (valueOfTile '(B 1) InitialState8X8))