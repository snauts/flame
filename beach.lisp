(defparameter *beach-walkable*
  '(130 138 146 154 162 170 178 186))

(defun beach-tile (id)
  (tile id :pl 1))

(defun beach-rocks (x1 y1 x2 y2)
  (crop x1 y1 x2 y2 (fill-box 16 8 (beach-tile 129))))

(defun beach-level ()
  (multiply (beach-rocks 0 6 8 8) 20))
