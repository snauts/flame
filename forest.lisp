(load "level.lisp")

(defparameter *forest-walkable*
  '(999))

(defun forest-walk (n)
  (multiply (stack (cell 999) 2) n))

(defun forest-level ()
  (join (forest-walk 256) (empty 64)))

(defun commit-save ()
  (push-level "forest_level" (forest-level))
  (save-level "forest.inc" *forest-walkable*))
