(load "level.lisp")

(defparameter *forest-walkable*
  '(139 147 155 163 171 179 187 195 203 211
    143 151 159 167 175 183 191 199
    214 222 230 238 206))

(defun forest-tile (id &key (v 0) (h 0) (pr 0))
  (tile id :pl 1 :v v :h h :pr pr))

(defun forest-cell (id &key (v 0) (h 0) (pr 0))
  (cell (forest-tile id :v v :h h :pr pr)))

(defun mud (x1 y1 x2 y2)
  (crop x1 y1 x2 y2 (fill-box 16 8 (forest-tile 137))))

(defun forest-walk-top (type)
  (case type
    (0 (mud 0 7 4 8))
    (1 (mud 4 7 8 8))
    (2 (mud 0 3 4 4))
    (3 (mud 4 3 8 4))
    (4 (mud 0 0 4 1))
    (5 (mud 4 0 8 1))))

(defun forest-walk-body (type)
  (case type
    (0 (mud 0 5 8 7))
    (1 (mud 0 1 8 3))))

(defun forest-walk-config (&key (top-L 0) (top-R 1) (body 0))
  (on-top (on-top (mud 0 4 8 5) (forest-walk-body body))
	  (join (forest-walk-top top-L) (forest-walk-top top-R))))

(defun forest-walk (&optional (n 1))
  (multiply (forest-walk-config) n))

(defun forest-puddle ()
  (forest-walk-config :top-L 2 :top-R 3 :body 1))

(defun platform-end-L ()
  (mud 8 4 9 8))

(defun platform-end-R ()
  (mud 9 4 10 8))

(defun log-segment (n walk)
  (forest-cell (set-walkable (+ 218 (mod n 3)) (= 0 walk))))

(defun forest-log (n &key base (walk -1) (top 217))
  (labels ((next () (forest-log (1- n) :walk (1- walk) :top top)))
    (cond ((numberp base) (on-top (forest-cell base) (next)))
	  ((> n 1) (on-top (log-segment n walk) (next)))
	  ((= n 1) (forest-cell top)))))

(defun horizontal-log-middle (n)
  (s-push (multiply (mud 10 2 12 3) (floor n 2)))
  (when (oddp n) (s-join (forest-cell 222)))
  (s-pop))

(defun horizontal-log (n)
  (join (forest-cell 227) (horizontal-log-middle n) (forest-cell 238)))

(defun platform-front ()
  (poke (mud 8 2 10 4) 0 0 0))

(defun forest-platform (n)
  (join (platform-front) (multiply (mud 10 2 12 4) n) (mud 12 2 14 4)))

(defun bridge-front (h)
  (place 0 h (forward (forest-log (+ h 4))) (forest-cell 206)))

(defun forest-bridge (n h)
  (let ((w (* 2 n)))
    (s-push (forest-log (+ h 5)))
    (s-place (- w 4) 0 (forest-log (+ h 5)))
    (s-place -2 h (forest-platform n))
    (s-place 4 0 (bridge-front h))
    (s-place w 0 (bridge-front h))
    (s-pop)))

(defun forest-start ()
  (join (forest-walk 2)
	(forest-puddle)
	(forest-walk)
	(platform-end-L)))

(defun puddle-walk ()
  (join (platform-end-R)
	(forest-walk-config :top-L 2 :top-R 1 :body 0)
	(forest-walk-config :top-L 0 :top-R 3 :body 0)
	(forest-walk-config :body 1)
	(platform-end-L)))

(defun sticky-walk ()
  (s-push (platform-end-R))
  (s-join (forest-walk-config :body 1 :top-L 2))
  (s-join (forest-walk-config :body 0 :top-L 2 :top-R 3))
  (s-join (forest-walk-config :body 1 :top-L 2 :top-R 1))
  (s-join (forest-walk-config :body 0 :top-L 0 :top-R 3))
  (s-place 26 0 (forward (forest-log 6 :walk 1 :top 226)))
  (s-place 16 0 (forward (forest-log 6 :walk 1)))
  (s-place 24 3 (forest-log 5 :base 200))
  (s-place 8 3 (forest-log 4 :base 200 :top 225))
  (s-place 6 3 (forest-log 3 :base 184))
  (s-join (platform-end-L))
  (s-pop))

(defparameter *wall-config*
  '((5  4 6 225) (6  4 5 225) (7  4 7 225) (8  4 8 225) (9 4 6 225)
    (10 4 7 225) (11 4 9 225) (15 4 8 225) (16 4 6 225)
    (12 4 3 217) (12 10 3 225 :base 228)
    (13 4 3 217) (13 10 4 225 :base 228)
    (14 4 3 217) (14 10 3 225 :base 228)
    (19 0 7 225 :walk 1)
    (20 0 9 225 :walk 1)
    (21 0 8 225 :walk 1)))

(defun draw-wall (wall-config)
  (dolist (config wall-config)
    (destructuring-bind (x y h top &key base (walk -1)) config
      (let ((tile (forest-log h :top top :base base :walk walk)))
	(s-place x y (if (< walk 0) tile (forward tile)))))))

(defun draw-fence ()
  (s-place 25 5 (horizontal-log 20))
  (loop for x from 28 to 44 by 8 do
    (s-place x 3 (forest-log 5 :top 226 :base 168))
    (s-place x 5 (forest-cell 206))))

(defun skull ()
  (poke (palette 0 (mud 8 0 11 2)) 0 1 0))

(defun skull-on-pole (h &rest stick-args)
  (s-push (apply #'forest-log (cons h stick-args)))
  (s-place -1 (1- h) (skull))
  (s-pop))

(defun skulls-on-sticks ()
  (s-push (platform-end-R))
  (s-join (forest-walk-config :body 0 :top-L 0 :top-R 3))
  (s-join (forest-walk-config :body 1 :top-L 2 :top-R 1))
  (s-join (forest-walk-config :body 1 :top-L 2 :top-R 3))
  (s-join (forest-walk-config :body 0 :top-L 0 :top-R 1))
  (s-place 8 3 (skull-on-pole 9 :base 200))
  (s-place 16 0 (forward (skull-on-pole 13 :walk 1)))
  (s-place 24 3 (flip (skull-on-pole 8 :base 200)))
  (s-join (platform-end-L))
  (s-pop))

(defun broken-wall ()
  (s-push (platform-end-R))
  (s-join (forest-walk-config :body 0 :top-L 2 :top-R 5))
  (s-join (forest-walk-config :body 1 :top-L 4 :top-R 5))
  (s-join (forest-walk-config :body 0 :top-L 0 :top-R 3))
  (s-join (forest-walk-config :body 0 :top-L 0 :top-R 1))
  (s-join (forest-walk-config :body 0 :top-L 2 :top-R 3))
  (s-join (forest-walk-config :body 1 :top-L 0 :top-R 1))
  (draw-wall *wall-config*)
  (s-join (platform-end-L))
  (draw-fence)
  (s-pop))

(defun forest-level ()
  (join ;; START
        (forest-start)
	(trigger "emit_mosquito")
	(empty 3)

	;; PART1
	(puddle-walk)
	(empty 3)
	(trigger "hole_emergers")

	;; PART2
	(inject (sticky-walk) "emit_bombers" 24)
	(empty 2)

	;; PART3
	(forest-bridge 8 2)
	(empty 2)
	(trigger "emit_jerkers")

	;; PART4
	(inject (broken-wall) "emit_divers" 19)
	(empty 3)

	;; PART5
	(skulls-on-sticks)
	(empty 3)

	;; ENDING
	(empty 64)))

(defun commit-save ()
  (push-level "forest_level" (forest-level))
  (save-level "forest.inc" *forest-walkable*))
