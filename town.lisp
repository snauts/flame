(load "level.lisp")

(defparameter *town-walkable*
  '(131 139 147 155 163 171 179 187))

(defun street-tile (id &key (v 0) (h 0) (pr 0))
  (tile id :pl 1 :v v :h h :pr pr))

(defun street-cell (id &key (v 0) (h 0) (pr 0))
  (cell (street-tile id :v v :h h :pr pr)))

(defun street (x1 y1 x2 y2)
  (crop x1 y1 x2 y2 (fill-box 16 8 (street-tile 129))))

(defun town-walk (n)
  (multiply (street 0 4 8 8) n))

(defun town-level ()
  (join (town-walk 16) (empty 64)))

(save-level
 "town.inc"
 *town-walkable*
 (list "town_level" (town-level)))
