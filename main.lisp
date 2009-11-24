(setf *red-image* "g:/lisp/lispgame/red.bmp")
(setf *blue-image* "g:/lisp/lispgame/blue.bmp")


(defun run ()
  (sdl:with-init ()
    (sdl:window 320 240)
    (sdl:update-display)
    (setf *grid-pic* (load-grid-pics))
    (sdl:with-events()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (when (SDL:KEY= KEY :SDL-KEY-ESCAPE) (SDL:PUSH-QUIT-EVENT)))
      (:idle ()
	     ;(sdl:draw-surface (sdl:load-image *red-image*))
	     ;(draw-grid *grid* 20 *grid-pic*)
	     (sdl:update-display))
      ;(:video-expose-event () (sdl:update-display))
      )
    )
  )

(defstruct color-block x y color)
(defstruct user-block b1 b2)

(defun draw-user-blocks ()
  
)

(defun draw-grid (grid gridsize pic-store)
  (loop for y from 0 to 3 do
       (loop for x from 0 to 5 do
	    (cond ((not (= (get_gridnum x y grid) 0))
		   (sdl:draw-surface-at-* 
		    (get-image (get_gridnum x y *grid*) pic-store) 
		    (* x 20) (* y 20))))
	    )
       )	    
  )

(defun load-grid-pics()
  (list (cons 1 (cons (sdl:load-image *red-image*) nil))
	(cons 2 (cons (sdl:load-image *blue-image*) nil))
	)
)

(defun get-image (image-num pic-store)
  (second (assoc image-num pic-store))
)