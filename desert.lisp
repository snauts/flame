(defun regular-ground ()
  (crop 0 0 8 4 (fill-box 8 8 (tile 97 :pl 1))))

(defun desert-level ()
  (serialize (multiply (regular-ground) 16)))
