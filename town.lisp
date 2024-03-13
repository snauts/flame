(load "level.lisp")

(defparameter *town-walkable*
  '(130 138 146 154))

(defun street-tile (id &key (v 0) (h 0) (pr 0))
  (tile id :pl 1 :v v :h h :pr pr))

(defun street-cell (id &key (v 0) (h 0) (pr 0))
  (cell (street-tile id :v v :h h :pr pr)))

(defun street (x1 y1 x2 y2)
  (crop x1 y1 x2 y2 (fill-box 16 8 (street-tile 129))))

(defun town-walk (n)
  (multiply (street 0 5 4 8) n))

(defun town-level ()
  (join (town-walk 32) (empty 64)))

(defun commit-save ()
  (push-level "town_level" (town-level))
  (save-level "town.inc" *town-walkable*))
