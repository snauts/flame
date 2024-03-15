(load "level.lisp")

(defparameter *town-walkable*
  '(130 138 146 154 162 170))

(defun street-tile (id &key (v 0) (h 0) (pr 0))
  (tile id :pl 1 :v v :h h :pr pr))

(defun street-cell (id &key (v 0) (h 0) (pr 0))
  (cell (street-tile id :v v :h h :pr pr)))

(defun street (x1 y1 x2 y2)
  (crop x1 y1 x2 y2 (fill-box 16 8 (street-tile 129))))

(defun walk-middle (n)
  (multiply (street 1 5 5 8) n))

(defun town-walk (n)
  (join (street 0 5 1 8) (walk-middle n) (street 5 5 6 8)))

(defun town-level ()
  (setf *seed* (* 1815 06 18))
  (join (town-walk 4)
	(empty 2)
	(town-walk 4)
	(empty 2)
	(town-walk 4)
	(empty 2)

	;; level done
	(inject  (town-walk 12) "level_done" 32)
	(empty 48)))

(defun commit-save ()
  (push-level "town_level" (town-level))
  (save-level "town.inc" *town-walkable*))
