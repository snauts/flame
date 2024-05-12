(load "level.lisp")

(defparameter *forest-walkable*
  '(139 147 155 163 171 179 187 195 203 211
    143 151 159 167 175 183 191 199))

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

(defun log-segment (n)
  (forest-cell (+ 218 (mod n 3))))

(defun forest-log (n)
  (cond ((= n 1) (forest-cell 217))
	((> n 1) (on-top (log-segment n) (forest-log (1- n))))))

(defun forest-level ()
  (join (forest-walk 2)
	(forest-puddle)
	(forest-walk)
	(platform-end-L)
	(empty 1)
	(forest-log 10)
	(empty 1)
	(platform-end-R)
	(forest-walk-config :top-L 2 :top-R 1 :body 0)
	(forest-walk-config :top-L 0 :top-R 3 :body 0)
	(forest-walk-config :body 1)
	(platform-end-L)
	(empty 64)))

(defun commit-save ()
  (push-level "forest_level" (forest-level))
  (save-level "forest.inc" *forest-walkable*))
