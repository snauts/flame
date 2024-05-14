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
    (3 (mud 4 3 8 4))))

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

(defun forest-log (n &key base (walk -1))
  (decf walk)
  (cond ((numberp base) (on-top (forest-cell base) (forest-log n :walk walk)))
	((> n 1) (on-top (log-segment n walk) (forest-log (1- n) :walk walk)))
	((= n 1) (forest-cell 217))))

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
  (s-place 26 0 (forward (forest-log 7 :walk 2)))
  (s-place 16 0 (forward (forest-log 6 :walk 2)))
  (s-place 24 3 (forest-log 5 :base 200))
  (s-place 8 3 (forest-log 4 :base 200))
  (s-place 6 3 (forest-log 3 :base 184))
  (s-join (platform-end-L))
  (s-pop))

(defun forest-level ()
  (join ;; START
        (forest-start)
	(trigger "emit_mosquito")
	(empty 3)

	;; PART1
	(puddle-walk)
	(empty 3)

	;; PART3
	(sticky-walk)
	(empty 3)

	;; PART2
	(forest-bridge 8 2)
	(empty 3)

	;; ENDING
	(empty 64)))

(defun commit-save ()
  (push-level "forest_level" (forest-level))
  (save-level "forest.inc" *forest-walkable*))
