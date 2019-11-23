
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

;;Tests if memList is part of second list 
(defun memberList (memList secondList) 
    (cond ((null secondList) '())
          ((equalp memList (car secondList)) t)
          (t(memberList memList (cdr secondList)))))

;;Joins two lists without duplicates
(defun join (firstList secondList)
    (cond ((null firstList) secondList)
          ((memberList (car firstList) secondList) (join (cdr firstList) secondList))
          (t(cons (car firstList) (join (cdr firstList) secondList)))))

;;Counts number of given element in the list
(defun countEl (element list) 
    (cond ((null list) 0)
          ((equal element (car list)) (+ 1 (count element (cdr list)))) 
          (t(count element (cdr list)))))

;;Inserts element in the list at givent index and pushing out previous element from that place
(defun insertElement (element list index)
    (cond ((null list) '())
          ((equal index 0) (cons element (cdr list)))
          (t(cons (car list) (insertElement element (cdr list) (- index 1))))))

;;Gets first N elements of passed list 
(defun getFirstN (numOfElements list) 
(cond ((or (null list) (> numOfElements (length list)) (= numOfElements 0)) '())
      (t(cons (car list) (getFirstN (- numOfElements 1) (cdr list))))))

;;Returnes last N elements of passed list
(defun getLastN (numOfElements list)
(cond ((null list) '())
      ((> numOfElements 0) (getLastN (- numOfElements 1) (cdr list)))
      (t (list EMPTY))))