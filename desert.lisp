(defun desert ()
  (fill-box 8 8 (tile 97 :pl 1)))

(defun cliffs ()
  (fill-box 9 8 (tile 161 :pl 1)))

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

(defun ground (&key (x1 0) (x2 8) (n 1))
  (multiply (crop x1 0 x2 4 (desert)) n))

(defun platform-edge (&key n f)
  (let ((edge (crop 8 (+ 1 (* n 4)) 9 (+ 3 (* n 4)) (cliffs))))
    (if (null f) edge (flip-h edge))))

(defun platform ()
  (let* ((platform (place 0 4 (crop 0 0 8 4 (cliffs)) (ground)))
	 (left-edge (place -1 5 platform (platform-edge :n 0 :f t))))
    (place 9 5 left-edge (platform-edge :n 1))))

(defun desert-level ()
  (serialize (join (aloe)
		   (ground :x2 2)
		   (cacti)
		   (ground :n 3)
		   (platform)
		   (ground :n 3)
		   (bush)
		   (empty 16))))
