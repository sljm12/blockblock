(defun deg->rad (x) (* x (/ pi 180)))

(defun 2d-rotate (x y deg)
  (let ((deg (deg->rad deg)))
    (list 
     (- (* x (cos deg)) (* y (sin deg)))
     (+ (* x (sin deg)) (* y (cos deg))))))