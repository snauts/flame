(load "level.lisp")

(defparameter *town-walkable*
  '(999))

(defun town-walk (n)
  (multiply (stack (cell 999) 2) n))

(defun town-level ()
  (join (town-walk 256) (empty 64)))

(defun save-town ()
  (with-open-file (out "town.inc" :if-exists :supersede :direction :output)
    (save-array out "town_level" (town-level) *town-walkable*)))

(defun save-and-quit ()
  (save-and-quit-level #'save-town))
