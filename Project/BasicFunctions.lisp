
;;This funcion returnes list that represents the printing stack from one tile in our board
(defun getTilePrintList (tileSize tileList) 
    (cond ((or (equalp tileSize 0) (null tileList)) '())
        ((> (- tileSize (length tileList)) 0) (cons '- (getTilePrintList (- tileSize 1) tileList)) )
        (t(cons (car tileList) (getTilePrintList (- tileSize 1) (cdr tileList))))))


;;This function returns sub list of a list
;;Arguments are list, first and last element of sublist
(defun getSubList (list firstElement lastElement) 
    (cond ((or (< firstElement 0) (> lastElement (length list)))
            (format t "First and last elements got to be in the range of given list"))
        ((null list) '())
        ((> firstElement 0) (getSubList (cdr list) (- firstElement 1) lastElement))
        ((> lastElement 0) (cons (car list) (getSubList (cdr list) firstElement (- lastElement 1))))))


;;Reads character from standard intput and puts in the list. Additionaly it shows message before reading.
(defun readChar (Message) (progn (format t  Message) (list(read))))

;;Returnes the index of element in list
(defun indexOf (element list)
    (cond ((equalp element (car list)) 0)
        (t(+ 1 (indexOf element (cdr list))))))

;;Returnes true if index of element is even
(defun evenIndexp (element list) (evenp (indexOf element list)))

;;Round number division
(defun div (divisor divident) (/ (- divisor (mod divisor divident)) divident))

;;Slices the list from given position and for given number of elements
