(defun regular-ground ()
  (crop 0 4 8 4 (make-rectangle 8 8 (tile 1 97))))

(defun desert-level ()
  (serialize-level (multiply-rectange (regular-ground) 16)))
