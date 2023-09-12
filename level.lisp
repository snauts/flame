(defmacro box-pipe (&rest forms)
  `(let ((pipe ,(first forms)))
     ,@(mapcar (lambda (x) `(setf pipe ,x)) (rest forms))
     pipe))

(defun tile (id &key (pr 0) (pl 0) (v 0) (h 0))
  (logior (ash pl 13) (ash pr 15) (ash v 12) (ash h 11) id))

(defun display-id (id)
  (cond ((null id) (format t "  .  "))
	((atom id) (format t "~4,' d " (logand id #x7ff)))
	(t (format t "~4,' d*" (logand (first id) #x7ff)))))

(defun width (box)
  (length box))

(defun height (box)
  (reduce #'max (mapcar #'length box)))

(defun empty (width)
  (make-list width :initial-element (list nil)))

(defun display (box)
  (loop for i from (1- (height box)) downto 0 do
    (dolist (column box)
      (display-id (nth i column)))
    (format t "~%")))

(defun integers (n &optional (x 0) (i 0))
  (unless (= i n) (cons (+ x i) (integers n x (1+ i)))))

(defun fill-box (x y tile)
  (let ((result nil))
    (dotimes (i x (reverse result))
      (push (reverse (integers y tile)) result)
      (incf tile y))))

(defun make (x y)
  (let ((result nil))
    (dotimes (i x result)
      (push (make-list y) result))))

(defun poke (box x y tile)
  (setf (elt (elt box x) y) tile))

(defun join (&rest rest)
  (apply #'append rest))

(defun for-all (box fn)
  (labels ((manipulate (x) (unless (null x) (funcall fn x))))
    (mapcar (lambda (column) (mapcar #'manipulate column)) box)))

(defun flip-horizontally (box)
  (for-all (reverse box) (lambda (x) (logior x (ash 1 11)))))

(defun place-in (a b i &optional result)
  (cond ((and (null a) (null b)) (reverse result))
	((> i 0) (place-in (rest a) b (1- i) (cons (first a) result)))
	(t (push (or (first b) (first a)) result)
	   (place-in (rest a) (rest b) 0 result))))

(defun place-one (x y a b result)
  (let ((aa (and (>= x 0) (first a)))
	(bb (and (<= x 0) (first b))))
    (cons (place-in aa bb y) result)))

(defun place (x y a b &optional result)
  (if (and (null a) (null b))
      (reverse result)
      (place (- x (signum x)) y
	     (if (< x 0) a (rest a))
	     (if (> x 0) b (rest b))
	     (place-one x y a b result))))

(defun cut (list a b)
  (butlast (nthcdr a list) (max 0 (- (length list) b))))

(defun crop (x1 y1 x2 y2 box)
  (mapcar (lambda (column) (cut column y1 y2)) (cut box x1 x2)))

(defun multiply (box n)
  (let ((result nil))
    (dotimes (i n result)
      (setf result (append box result)))))

(defun remove-meta (tile)
  (if (consp tile) (first tile) tile))

(defun zero-first (box)
  (mapcar #'remove-meta (substitute 0 nil (first box))))

(defun index-pair (columns prev)
  (logior (or (length (first columns)) 0)
	  (ash (or (length prev) 0) 8)))

(defun serialize (box &optional prev flat)
  (labels ((add (x) (push x flat)))
    (add (index-pair box prev))
    (mapc #'add (zero-first box))
    (if (null box)
	(reverse flat)
	(serialize (rest box) (first box) flat))))

(defun save-words (out words)
  (let ((count 0))
    (dolist (w words)
      (format out "0x~4,'0X, " w)
      (when (>= (incf count) 8)
	(format out "~%")
	(setf count 0)))
    (when (/= count 0)
      (format out "~%"))))

(defun save-array (out name level)
  (format out "const u16 ~A[] = {~%" name)
  (save-words out (serialize level))
  (format out "};~%"))

(load "desert.lisp")

(defun save-level ()
  (with-open-file (out "level.inc" :if-exists :supersede :direction :output)
    (save-array out "desert_level" (desert-level))))

(defun save-and-quit ()
  (handler-case (save-level)
    (condition (var) (format t "ERROR: ~A~%" var)))
  (quit))
