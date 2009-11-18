(setf *red-image* "f:/lisp/lispgame/red.bmp")



(defun run ()
  (sdl:with-init ()
    (sdl:window 320 240)
    (sdl:update-display)
    (setf *red* (sdl:load-image *red-image*))
    (sdl:with-events()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (when (SDL:KEY= KEY :SDL-KEY-ESCAPE) (SDL:PUSH-QUIT-EVENT)))
      (:idle ()
	     ;(sdl:draw-surface (sdl:load-image *red-image*))
	     (draw-grid *grid* 20)
	     (sdl:update-display))
      ;(:video-expose-event () (sdl:update-display))
      )
    )
  )

(defun draw-grid (grid gridsize)
  (loop for y from 0 to 3 do
       (loop for x from 0 to 5 do
	    (cond ((not (= (get_gridnum x y grid) 0))
		   (sdl:draw-surface-at-* *red* (* x 20) (* y 20))))
	    )
       )	    
  )