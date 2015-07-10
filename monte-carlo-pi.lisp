;;; A basic monte-carlo simulation for approximating the value of pi.

(defun random-point ()
  "Generate a random point in the range [0, 1)."
  (cons (random 1.0)
	(random 1.0)))

(defun in-circle (point)
  "Return true if the given point is within the circle boundary."
  (let ((x (car point))
	(y (cdr point)))
    (< (sqrt (+ (* x x) (* y y))) 1)))

(defun run-sim (iterations)
  "Run the simulation for so many iterations."
  (declare (optimize (speed 3) (debug 0)))
  (let ((good-ones 0))
    (dotimes (i iterations)
	(when (in-circle (random-point))
	  (incf good-ones)))
    (* 4.0 (/ good-ones iterations))))
