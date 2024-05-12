(load "level.lisp")

(defparameter *forest-walkable*
  '(139 147 155 163 171 179 187 195))

(defun forest-tile (id &key (v 0) (h 0) (pr 0))
  (tile id :pl 1 :v v :h h :pr pr))

(defun forest-cell (id &key (v 0) (h 0) (pr 0))
  (cell (forest-tile id :v v :h h :pr pr)))

(defun mud (x1 y1 x2 y2)
  (crop x1 y1 x2 y2 (fill-box 16 8 (forest-tile 137))))

(defun forest-walk (n)
  (multiply (mud 0 4 8 8) n))

(defun forest-level ()
  (join (forest-walk 16) (empty 64)))

(defun commit-save ()
  (push-level "forest_level" (forest-level))
  (save-level "forest.inc" *forest-walkable*))
