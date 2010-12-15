(setf *red-image* "c:/lisp/lispgame/red.bmp")
(setf *blue-image* "c:/lisp/lispgame/blue.bmp")


(defun run ()
  (let ((ub (make-user-block :b1 (make-color-block :x 1 :y 1 :color 1)
			     :b2 (make-color-block :x 1 :y 1 :color 1)))
	;(*grid-pic* (load-grid-pics))
	)
	     
  (sdl:with-init ()
    (sdl:window 320 240)
    (sdl:update-display)
    (setf *grid-pic* (load-grid-pics))
    
    (sdl:with-events()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (when (SDL:KEY= KEY :SDL-KEY-ESCAPE) (SDL:PUSH-QUIT-EVENT)))
      (:idle ()
	     (draw-grid *grid* 20 *grid-pic*)
	     (draw-user-blocks ub *grid-pic*)
	     (sdl:update-display))
      )
    ))
  )

(defstruct color-block x y color)
(defstruct user-block b1 b2)

(defun draw-user-blocks (user-block pic-store)
  (let ((b1 (user-block-b1 user-block))
	(b2 (user-block-b2 user-block)))
    (if (not (eq b1 nil))
	(draw-block b1 pic-store))
    (if (not (eq b2 nil))
	(draw-block b2 pic-store))
    ))

(defun draw-block (c-block pic-store)
  (sdl:draw-surface-at-* 
   (get-image (color-block-color c-block)  pic-store) 
   (color-block-x c-block) 
   (color-block-y c-block)))

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