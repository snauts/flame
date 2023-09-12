(defun desert ()
  (fill-box 8 8 (tile 97 :pl 1)))

(defun plant (x y w h)
  (let ((plant (crop x y (+ x w) (+ y h) (desert)))
	(under (crop x 0 (+ x w) 2 (desert))))
    (place 0 2 under plant)))

(defun bush ()
  (plant 2 4 6 2))

(defun aloe ()
  (plant 2 6 6 2))

(defun cacti ()
  (plant 0 4 2 4))

(defun ground (&key (x1 0) (x2 8))
  (crop x1 0 x2 4 (desert)))

(defun desert-level ()
  (serialize (join (aloe)
		   (ground :x2 2)
		   (cacti)
		   (multiply (ground) 7)
		   (bush)
		   (empty 16))))
