
;Grid operations

(setf *grid* (make-array '(4 6) :initial-contents 
			 '((0 1 1 1 0 0) 
			   (0 0 0 1 1 1) 
			   (1 1 1 1 1 0)
			   (1 1 1 1 0 0)
			   )
			 )
      )

(defun pretty-print-grid (grid)
  (do ( (y 0 (1+ y)))
      ( (= y (array-dimension grid 0)))
    (print (get-array-row grid y 0))
    )
  )

;NOTE: Not used
(defun get-array-row (grid y current-x)
  (let ( (max-x  (array-dimension grid 1)))
    (if (= current-x max-x) '() 
	;(append (get-array-row grid y (1- current-x)) (aref grid y current-x))
	(cons (aref grid y current-x) (get-array-row grid y (1+ current-x)))
	)
    
    )
  )

;Get the number from the grid    
(defun get_gridnum (x y grid)
  (aref grid y x))

;Set the number into the grid
(defun set_gridnum (x y value grid)
  (setf (aref grid y x) value))

(defun get-rows-linked-grid (y linked-grid)
  (second (assoc y linked-grid)))

;scan-grid will scan the grid at a given co-ord to look for numbers that are the same and
;joined together
;returns a list of (y (min-x max-x)) to denote where the similar numbers are found
(defun get-linked-grids (x y grid)
  (let ((grid-num (get_gridnum x y grid))
	)
    (scan-rows x grid-num grid (scan-column x y grid-num grid))
    )
  )

;Gives back a range fof numbers from (start .. (1- end)) ie. (range 1 5) = (1 2 3 4)
(defun range (start end)
  (loop for i from start below end collect i))


;returns range of y where grid-num is correct
(defun scan-column (x y grid-num grid)
  (if (not (= (get_gridnum x y grid) grid-num)) nil
      (range (scan-up x y grid-num grid) (1+ (scan-down x y grid-num grid)))
      )
)

;maps the column-range to scan-one-row
(defun scan-rows (x grid-num grid column-range)
  (mapcar #'(lambda (y) (scan-one-row x y grid-num grid)) column-range)
)

;retuns nil if the grid num for x y does not match
;else returns (y (range min-x max-x)) where grid-num matches
(defun scan-one-row (x y grid-num grid)
  (if (not (= (get_gridnum x y grid) grid-num)) nil
     (list y (range (scan-left x y grid-num grid) (1+ (scan-right x y grid-num grid))))
    )
)

(defun scan-left (x y grid-num grid)
  (let ((current-grid (get_gridnum x y grid)))
    (cond
      ( 
       (not (= current-grid grid-num)) 
       (+ x 1)
       )
      ( 
       (= current-grid grid-num)
       (if 
	(= x 0) 0 
	(scan-left (- x 1) y grid-num grid)
	)
       )
      )
    )
)

(defun scan-right (x y grid-num grid)
  (let ( 
	(current-grid (get_gridnum x y grid))
	(limit (- (array-dimension grid 1) 1));Set upper bound of row
	)
    (cond 
      ( (not (= current-grid grid-num))
       (- x 1))
      ((= current-grid grid-num)
       (if (= x limit) limit
	   (scan-right (+ x 1) y grid-num grid))
       )
      )
    )
)

(defun scan-up (x y grid-num grid)
  (let
      (
       (current-grid (get_gridnum x y grid))
       )
    (cond
      ( (not (= current-grid grid-num))
       (+ y 1)
	)
      ( (= current-grid grid-num)
       (if (= y 0) 0
	   (scan-up x (- y 1) grid-num grid)
	   )
	)
      )))

(defun scan-down (x y grid-num grid)
  (let
      (
       (current-grid (get_gridnum x y grid))
       (max-y (1- (array-dimension grid 0) ))
       )
    (cond
      ( (not (= current-grid grid-num))
       (- y 1)
	)
      ( (= current-grid grid-num)
       (if (= y max-y) max-y
	   (scan-down x (+ y 1) grid-num grid)
	   )
	)
      )))
