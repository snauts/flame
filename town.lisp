(load "level.lisp")

(defparameter *town-walkable*
  '(130 138 146 154 162 170 136 144 152))

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

(defun random-cell (&rest choices)
  (street-cell (elt choices (xor-random (length choices)))))

(defun bottom-boulder ()
  (join (random-cell 142 158) (random-cell 150 166)))

(defun wall-bottom-top (n)
  (s-push nil)
  (dotimes (i n (s-pop))
    (s-join (bottom-boulder))))

(defun wall-bottom-middle (w)
  (on-top (multiply (street 1 1 5 2) w) (wall-bottom-top (* 2 w))))

(defun wall-bottom (w)
  (join (street 0 1 1 3) (wall-bottom-middle w) (street 5 1 8 3)))

(defun small-brick ()
  (if (= 0 (xor-random 2))
      (street-cell 141)
      (street-cell 165)))

(defun large-brick-left ()
  (case (xor-random 3)
    (0 (street-cell 140))
    (1 (street-cell 156))
    (2 (street-cell 149))))

(defun large-brick-right ()
  (case (xor-random 3)
    (0 (street-cell 148))
    (1 (street-cell 164))
    (2 (street-cell 157))))

(defun large-brick ()
  (join (large-brick-left)
	(large-brick-right)))

(defun large-brick-row (n)
  (s-push nil)
  (dotimes (i n (s-pop))
    (s-join (large-brick))))

(defun brick-bottom-row (n)
  (join (small-brick) (large-brick-row n) (small-brick)))

(defun brick-row (n)
  (on-top (brick-bottom-row (1- n)) (large-brick-row n)))

(defun wall-row (w)
  (join (street 0 3 1 5) (brick-row (* 2 w)) (street 5 3 7 5)))

(defun stack-cells (&rest cells)
  (reduce #'on-top (mapcar #'street-cell cells)))

(defun shingles (n)
  (let ((middle (multiply (stack-cells 144 168) n)))
    (join (stack-cells 136 160) middle (stack-cells 152 176))))

(defun town-wall (w h)
  (s-push (wall-bottom w))
  (loop for y from 2 to (* 2 h) by 2 do
    (s-place 0 y (wall-row w)))
  (s-place 0 (* 2 (1+ h)) (shingles (1+ (* 4 w))))
  (s-pop))

(defun lamp-post (&key (base-h 3) (post-h 6) walkable)
  (s-push (stack (street-cell 179) base-h))
  (when (numberp walkable)
    (s-place 0 walkable (street-cell (set-walkable 179))))
  (s-place-top 0 (street-cell 178))
  (s-place-top 0 (stack (street-cell 177) post-h))
  (s-place-top -1 (street 7 5 10 6))
  (let ((shine (palette 0 (street 7 6 10 7))))
    (s-place-top 0 shine)
    (s-place-top 0 (topple shine)))
  (s-place-top 0 (street 7 7 10 8))
  (s-pop))

(defun town-level ()
  (setf *seed* (* 1815 06 18))
  (join (town-walk 4)
	(empty 2)
	(lower (town-wall 2 2) 2)
	(lamp-post)
	(empty 1)
	(place 5 2 (town-walk 6) (town-wall 3 1))
	(empty 2)

	;; level done
	(inject  (town-walk 12) "level_done" 32)
	(empty 48)))

(defun commit-save ()
  (push-level "town_level" (town-level))
  (save-level "town.inc" *town-walkable*))
