(defparameter *alps-walkable*
  '(67 75 83 91 99 107 115 123 71 79))

(defun alpine-tile (id)
  (tile id :pl 1))

(defun rocks ()
  (fill-box 16 8 (alpine-tile 65)))

(defun two-leaf ()
  (crop 0 3 2 4 (rocks)))

(defun small-plant ()
  (crop 2 2 6 4 (rocks)))

(defun flower-bush ()
  (crop 2 0 6 2 (rocks)))

(defun tubular-flower ()
  (crop 8 4 12 7 (rocks)))

(defun hanging-flower ()
  (crop 12 4 16 7 (rocks)))

(defun tall-plant ()
  (crop 8 0 12 4 (rocks)))

(defun cup-flower ()
  (crop 12 0 16 2 (rocks)))

(defun iris-flower ()
  (crop 12 2 16 4 (rocks)))

(defun narrow-rock-platform ()
  (box-pipe
   (place 0 2 pipe (crop 6 2 8 4 (rocks)))
   (place 0 0 pipe (crop 0 4 1 6 (rocks)))
   (place 1 0 pipe (crop 7 4 8 6 (rocks)))))

(defun hanging-rock-platform ()
  (place 0 0 (crop 0 4 8 8 (rocks)) (crop 8 7 16 8 (rocks))))

(defun alpine-base (base)
  (case base
    (0 (crop 0 4 8 8 (rocks)))
    (1 (narrow-rock-platform))
    (2 (hanging-rock-platform))
    (3 (crop 0 4 4 8 (rocks)))
    (4 (crop 4 4 8 8 (rocks)))))

(defun alpine-plants (rocks type)
  (case type
    (0 rocks)
    (1 (place 2 3 rocks (two-leaf)))
    (2 (place 4 3 rocks (small-plant)))
    (3 (place 0 3 rocks (flower-bush)))
    (4 (place 0 3 rocks (tubular-flower)))
    (5 (place 4 3 rocks (hanging-flower)))
    (6 (place 0 3 rocks (tall-plant)))
    (7 (place 4 3 rocks (cup-flower)))
    (8 (place 4 3 rocks (iris-flower)))))

(defun alpine-vegetation (rocks type)
  (cond ((null type) rocks)
	((numberp type)
	 (alpine-plants rocks type))
	((consp type)
	 (alpine-vegetation (alpine-plants rocks (first type)) (rest type)))
	(t (error "ALPINE-PLANT bad type"))))

(defun alpine-left-side (base)
  (place 0 2 (crop 0 0 1 3 (rocks)) (when (= base 4) (crop 6 0 7 2 (rocks)))))

(defun alpine-right-side (base)
  (place 0 2 (crop 1 0 2 3 (rocks)) (when (= base 3) (crop 7 0 8 2 (rocks)))))

(defun alps-walk (&key (width 1) (type 0) (base 0))
  (let ((rocks (alpine-vegetation (alpine-base base) type)))
    (join (alpine-left-side base)
	  (multiply rocks width)
	  (alpine-right-side base))))

(defun mountain-level ()
  (join
   (alps-walk :width 3)
   (empty 2)
   (alps-walk :width 1 :base 3)
   (empty 2)
   (alps-walk :width 1 :base 4)
   (empty 2)
   (alps-walk :width 1 :type 1)
   (empty 2)
   (alps-walk :width 1 :type 2)
   (trigger "emit_bee_stream")
   (empty 2)
   (alps-walk :width 1 :type 3)
   (empty 2)
   (alps-walk :width 1 :type 4)
   (empty 2)
   (alps-walk :width 1 :type 5)
   (empty 2)
   (alps-walk :width 1 :type 6)
   (empty 2)
   (alps-walk :width 1 :type 7)
   (empty 2)
   (alps-walk :width 1 :type 8)
   (empty 2)
   (alps-walk :width 1 :base 1)
   (empty 2)
   (place 0 3 (empty 1) (alps-walk :width 1 :type '(2 3) :base 2))
   (empty 2)
   (place 0 5 (empty 1) (alps-walk :width 1 :type '(2 4) :base 2))
   (empty 2)
   (place 0 7 (empty 1) (alps-walk :width 1 :type '(3 5) :base 2))
   (empty 2)
   (place 0 9 (empty 1) (alps-walk :width 1 :type '(6 7) :base 2))
   (empty 2)
   (place 0 11 (empty 1) (alps-walk :width 1 :type '(4 8) :base 2))
   (empty 64)))
