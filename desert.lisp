(defun desert ()
  (fill-box 8 8 (tile 97 :pl 1)))

(defun cacti ()
  (let ((cacti (crop 0 4 2 8 (desert)))
	(under (crop 0 0 2 2 (desert))))
    (place 0 2 under cacti)))

(defun ground ()
  (crop 0 0 8 4 (desert)))

(defun desert-level ()
  (serialize (join (cacti) (multiply (ground) 16))))
