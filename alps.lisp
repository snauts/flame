(defparameter *alps-walkable*
  '(67 75 83 91 99 107 115 123 71 79))

(defun alpine-tile (id)
  (tile id :pl 1))

(defun rocks (x1 y1 x2 y2)
  (crop x1 y1 x2 y2 (fill-box 16 8 (alpine-tile 65))))

(defun two-leaf ()
  (rocks 0 3 2 4))

(defun small-plant ()
  (rocks 2 2 6 4))

(defun flower-bush ()
  (rocks 2 0 6 2))

(defun tubular-flower ()
  (rocks 8 4 12 7))

(defun hanging-flower ()
  (rocks 12 4 16 7))

(defun tall-plant ()
  (rocks 8 0 12 4))

(defun cup-flower ()
  (rocks 12 0 16 2))

(defun iris-flower ()
  (rocks 12 2 16 4))

(defun narrow-rock-platform ()
  (box-pipe
   (place 0 2 pipe (rocks 6 2 8 4))
   (place 0 0 pipe (rocks 0 4 1 6))
   (place 1 0 pipe (rocks 7 4 8 6))))

(defun hanging-rock-platform ()
  (place 0 0 (rocks 0 4 8 8) (rocks 8 7 16 8)))

(defun alpine-solid (base)
  (case base
    (0 (rocks 0 4 8 8))
    (1 (rocks 0 4 4 8))
    (2 (rocks 4 4 8 8))
    (3 (narrow-rock-platform))))

(defparameter *solid-to-hanging*
  '((68  129) (76  137) (84  145) (92  153)
    (100 161) (108 169) (116 177) (124 185)))

(defun hanging (solid)
  (exchange solid *solid-to-hanging*))

(defun alpine-base (base hang)
  (let ((solid (alpine-solid base)))
    (if (/= hang 0) (hanging solid) solid)))

(defun alpine-plants (rocks type offset)
  (case type
    (0 rocks)
    (1 (place (+ offset 2) 3 rocks (two-leaf)))
    (2 (place offset 3 rocks (flower-bush)))
    (3 (place offset 3 rocks (tubular-flower)))
    (4 (place offset 3 rocks (tall-plant)))
    (5 (place offset 3 rocks (small-plant)))
    (6 (place offset 3 rocks (hanging-flower)))
    (7 (place offset 3 rocks (cup-flower)))
    (8 (place offset 3 rocks (iris-flower)))))

(defun alpine-vegetation (rocks type &optional (offset 0))
  (cond ((null type) rocks)
	((numberp type)
	 (alpine-plants rocks type offset))
	((consp type)
	 (let ((result (alpine-plants rocks (first type) offset)))
	   (alpine-vegetation result (rest type) (+ offset 4))))
	(t (error "ALPINE-PLANT bad type"))))

(defun alpine-left-side (base)
  (place 0 2 (rocks 0 0 1 3) (when (= base 2) (rocks 6 0 7 2))))

(defun alpine-right-side (base)
  (place 0 2 (rocks 1 0 2 3) (when (= base 1) (rocks 7 0 8 2))))

(defun alps-walk (&key (width 1) (type 0) (base 0) (hang 0))
  (let ((rocks (multiply (alpine-base base hang) width)))
    (join (alpine-left-side base)
	  (alpine-vegetation rocks type)
	  (alpine-right-side base))))

(defun mountain-level ()
  (join
   (alps-walk :width 3)

   ;; PART 1
   (empty 2)
   (alps-walk :width 2 :type '(1 5 2 5))
   (empty 2)
   (trigger "emit_bee_block")
   (alps-walk :type '(2 5))

   ;; PART 2
   (empty 2)
   (raise 3  (alps-walk :type 1 :hang 1 :base 1))
   (empty 2)
   (raise 6  (alps-walk :type 2 :hang 1 :base 1))
   (empty 2)
   (raise 9  (alps-walk :type 6 :hang 1 :base 2))
   (empty 2)
   (raise 12 (alps-walk :type 3 :hang 1 :base 1))
   (empty 2)
   (raise 9  (alps-walk :type 7 :hang 1 :base 2))
   (empty 2)
   (raise 6  (alps-walk :type 4 :hang 1 :base 1))
   (empty 2)
   (raise 3  (alps-walk :type 8 :hang 1 :base 2))
   (empty 64)))
