(setf *red-image* "c:/lisp/blockblock/red.bmp")
(setf *blue-image* "c:/lisp/blockblock/blue.bmp")


(defun run ()
  (let ((ub (make-user-block :b1 (make-color-block :x 1 :y 1 :color 1)
			     :b2 (make-color-block :x 2 :y 1 :color 1)
			     :r-state 1))
	(grid-parameters (make-grid-parameters :block-size 20 :startx 0 :starty 0 :grid-size-x 10 :grid-size-y 10))
	)
	     
  (sdl:with-init ()
    (sdl:window 320 240)
    (sdl:update-display)
    (setf *grid-pic* (load-grid-pics))
    
    (sdl:with-events()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (when 
			   (SDL:KEY= KEY :SDL-KEY-ESCAPE) 
			 (SDL:PUSH-QUIT-EVENT))
		       (when (SDL:KEY= KEY :SDL-KEY-LEFT)
			 (rotate-user-blocks 1 ub)
			 (print ub))
		       (when (SDL:KEY= KEY :SDL-KEY-RIGHT)
			 (rotate-user-blocks 1 ub)))
      (:idle ()
	     (sdl:clear-display sdl:*black*)
	     (draw-grid *grid* 20 *grid-pic*)
	     (draw-user-blocks ub *grid-pic* grid-parameters)
	     (sdl:update-display))
      )
    )))

;Standard parameters for the grid
(defstruct grid-parameters block-size startx starty grid-size-x grid-size-y)
;Define one block
(defstruct color-block x y color)
;the user blocks consists of 2 color-block
(defstruct user-block b1 b2 r-state)

(defun draw-user-blocks (user-block pic-store grid-parameters)
  (let ((b1 (user-block-b1 user-block))
	(b2 (user-block-b2 user-block)))
    (if (not (eq b1 nil))
	(draw-block b1 pic-store grid-parameters))
    (if (not (eq b2 nil))
	(draw-block b2 pic-store grid-parameters))
    ))

(defun draw-block (c-block pic-store grid-parameters)
  (let ((block-size (grid-parameters-block-size grid-parameters))
	(start-x (grid-parameters-startx grid-parameters))
	(start-y (grid-parameters-starty grid-parameters)))
    (sdl:draw-surface-at-* 
     (get-image (color-block-color c-block)  pic-store) 
     (+ start-x (* (color-block-x c-block) block-size))
     (+ start-y (* (color-block-y c-block) block-size)))))

(defun rotate-user-blocks (direction user-block)
  (cond ((= direction 1)
	 (when (need-rotate? user-block) (rotate-left-user-block user-block)))
	)
  )

(defun rotate-user-block (user-block fn) 
  (let* ((b2 (user-block-b2 user-block))
	 (b1 (user-block-b1 user-block))
	 (newx 0)
	 (newy 0)
	 (originx (color-block-x b1))
	 (originy (color-block-y b1))
	 (r-state (mod (#'fn (user-block-r-state user-block)) 4)))
    (case r-state 
      ((0)
       (setf newx (+ originx 1))
       (setf newy originy))
      ((1)
       (setf newx originx)
       (setf newy (- originy 1)))
      ((2)
       (setf newx (- originx 1))
       (setf newy originy))
      ((3)
       (setf newx originx)
       (setf newy (+ originy 1))))
    (setf (color-block-x b2)newx )
    (setf (color-block-y b2) newy)
    (setf (user-block-r-state user-block) r-state)
  ))

(defun rotate-color-block (originx originy color-block angle)
  (let* ((x (color-block-x color-block))
	 (y (color-block-y color-block))
	 (result (2d-rotate originx originy x y angle)))
    (setf (color-block-x color-block) (truncate (first result)))
    (setf (color-block-y color-block) (truncate (second result)))
    color-block))

(defun rotate-left-color-block (originx originy color-block)
  (rotate-color-block originx originy color-block -90))

(defun rotate-right-color-block (originx originy color-block)
  (rotate-color-block originx originy color-block 90))

(defun need-rotate? (user-block)
  (if (or (eq (user-block-b1 user-block) nil)
	  (eq (user-block-b2 user-block) nil))
      nil t))

(defun draw-grid (grid gridsize pic-store)
  (map-draw-grid 3 3 #'draw-grid-at (list grid pic-store 100 100))
  )

;Draw a grid to the screen
;grid-x, grid-y the x and y of the grid that is to be drawn
;grid the array that stores the grid
;pic-store the picture store 
;startx,starty the x,y position to start drawing the grid
(defun draw-grid-at (grid-x grid-y grid pic-store startx starty)
  (cond ((not (= (get_gridnum grid-x grid-y grid) 0))
		 (sdl:draw-surface-at-* 
		  (get-image (get_gridnum grid-x grid-y *grid*) pic-store) 
		  (+ (* grid-x 20) startx) 
		  (+ (* grid-y 20) starty)))))



;Higher order function for looping through the map grid, grid starts from 0
;Max-x is the max size of the grid
;Max-y is the max size of the grid in y
;fn to execute minimium needs to have 2 parameters with the first being x and the second y
;fn-parameters additional parameters to pass to fn
(defun map-draw-grid (max-x max-y fn fn-parameters)
	(loop for y from 0 to max-y do
		 (loop for x from 0 to max-x do
			  (apply fn (append (list x y) fn-parameters))
			  )
		 ))



(defun load-grid-pics()
  (list (cons 1 (cons (sdl:load-image *red-image*) nil))
	(cons 2 (cons (sdl:load-image *blue-image*) nil))
	)
)

(defun get-image (image-num pic-store)
  (second (assoc image-num pic-store))
)