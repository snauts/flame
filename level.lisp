(defmacro box-pipe (&rest forms)
  `(let ((pipe (empty 1)))
     ,@(mapcar (lambda (x) `(setf pipe ,x)) forms)
     pipe))

(defmacro with-box (box &rest forms)
  `(progn
     ,@(mapcar (lambda (x) `(setf ,box ,x)) forms)
     ,box))

(defmacro top-pipe (&rest forms)
  `(let ((pipe ,(first forms)))
     ,@(mapcar (lambda (x) `(setf pipe (on-top pipe ,x))) (rest forms))
     pipe))

(defparameter *seed* 0)

(defun xor-seed (shift)
  (setf *seed* (logxor *seed* (ash *seed* shift))))

(defun xor-random (x)
  (xor-seed 13)
  (xor-seed -7)
  (xor-seed 17)
  (mod *seed* x))

(defun tile (id &key (pr 0) (pl 0) (v 0) (h 0))
  (logxor (ash pr 15) (ash v 12) (ash h 11) (logior (ash pl 13) id)))

(defun idx (tile)
  (if tile (logand tile #x7ff) 0))

(defun display-id (tile)
  (if (null tile)
      (format t "  .  ")
      (format t "~4,' d " (idx tile))))

(defun width (box)
  (length box))

(defun height (box)
  (if (null box) 0 (reduce #'max (mapcar #'length box))))

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

(defun make (x y &key e)
  (let ((result nil))
    (dotimes (i x result)
      (push (make-list y :initial-element e) result))))

(defun cell (id)
  (make 1 1 :e (tile id)))

(defun join (&rest rest)
  (apply #'append rest))

(defun trigger (name)
  (list name))

(defun inject (box name pos)
  (if (= 0 pos)
      (cons name box)
      (cons (first box) (inject (rest box) name (1- pos)))))

(defun for-all (box fn)
  (labels ((manipulate (x) (unless (null x) (funcall fn x))))
    (mapcar (lambda (column) (mapcar #'manipulate column)) box)))

(defun flip (box)
  (for-all (reverse box) (lambda (x) (logxor x (ash 1 11)))))

(defun topple (box)
  (mapcar #'reverse (for-all box (lambda (x) (logxor x (ash 1 12))))))

(defun forward (box)
  (for-all box (lambda (x) (logior x (ash 1 15)))))

(defun palette (pl box)
  (for-all box (lambda (x) (logior (logand x #x9fff) (ash pl 13)))))

(defun params (tile)
  (if tile (logand tile (lognot #x7ff)) 0))

(defun look-up (x table)
  (let ((entry (assoc (idx x) table)))
    (if entry (logior (params x) (second entry)) x)))

(defun exchange (box table)
  (for-all box (lambda (x) (look-up x table))))

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

(defun raise (h box)
  (place 0 h nil box))

(defun poke (box x y tile)
  (place x y box (make 1 1 :e tile)))

(defun cut (list a b)
  (butlast (nthcdr a list) (max 0 (- (length list) b))))

(defun crop (x1 y1 x2 y2 box)
  (mapcar (lambda (column) (cut column y1 y2)) (cut box x1 x2)))

(defun multiply (box n)
  (let ((result nil))
    (dotimes (i n result)
      (setf result (append box result)))))

(defun stack (box n)
  (let ((result nil))
    (dotimes (i n result)
      (setf result (place 0 (* i (height box)) result box)))))

(defun on-top (box1 box2)
  (place 0 (height box1) box1 box2))

(defun zero-first (box)
  (substitute 0 nil (first box)))

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

(defun column-height (column walkable &optional result (height 224))
  (decf height 8)
  (cond ((null column) result)
	((member (idx (first column)) walkable)
	 (column-height (rest column) walkable (cons height result) height))
	(t (column-height (rest column) walkable result height))))

(defun height-map (box walkable)
  (mapcar (lambda (column) (column-height column walkable)) box))

(defun inc-distance (compacted)
  (incf (first (first compacted)))
  compacted)

(defun should-new-entry (raw head)
  (or (null head)
      (<= 255 (first head))
      (not (equal (first raw) (rest head)))))

(defun compact (raw compacted)
  (cond ((null raw) (reverse compacted))
	((should-new-entry raw (first compacted))
	 (compact (rest raw) (cons (cons 1 (first raw)) compacted)))
	(t (compact (rest raw) (inc-distance compacted)))))

(defun encode-map (map &optional flat (prev 0))
  (let ((size (length (first map))))
    (push (logior size (ash prev 4)) flat)
    (mapc (lambda (x) (push x flat)) (first map))
    (if (null map)
	(reverse flat)
	(encode-map (rest map) flat size))))

(defun encode-height (level walkable)
  (encode-map (compact (height-map level walkable) nil)))

(defun out-format (n)
  (concatenate 'string "0x~" (format nil "~d" n) ",'0X, "))

(defun save-hex (out data &optional (n 4))
  (let ((count 0))
    (dolist (d data)
      (format out (out-format n) d)
      (when (>= (incf count) 8)
	(format out "~%")
	(setf count 0)))
    (when (/= count 0)
      (format out "~%"))))

(defun save-trigger-entry (out distance name &optional (prefix ""))
  (format out "{ distance: 0x~4,'0x, fn: ~A~A },~%" distance prefix name))

(defun save-triggers (out level &optional (distance 0))
  (cond ((null level)
	 (save-trigger-entry out 0 "NULL"))
	((stringp (first level))
	 (save-trigger-entry out distance (first level) "&")
	 (save-triggers out (rest level) distance))
	(t (save-triggers out (rest level) (+ 8 distance)))))

(defun save-declarations (out names)
  (mapc (lambda (x) (format out "void ~A(u16);~%" x)) names))

(defun save-array (out name level walkable)
  (let ((clean (remove-if #'stringp level)))
    (save-declarations out (remove-if-not #'stringp level))
    (format out "const Trigger ~A_triggers[] = {~%" name)
    (save-triggers out level)
    (format out "};~%")
    (format out "const byte ~A_height[] = {~%" name)
    (save-hex out (encode-height clean walkable) 2)
    (format out "};~%")
    (format out "const u16 ~A[] = {~%" name)
    (save-hex out (serialize clean))
    (format out "};~%")))

(load "desert.lisp")
(load "alps.lisp")

(defun save-level ()
  (with-open-file (out "level.inc" :if-exists :supersede :direction :output)
    (save-array out "mountain_level" (mountain-level) *alps-walkable*)
    (save-array out "desert_level" (desert-level) *desert-walkable*)
    (save-array out "mantis_level" (mantis-level) *desert-walkable*)
    (save-array out "rusty_level" (rusty-level) *desert-walkable*)))

(defun save-and-quit ()
  (handler-case (save-level)
    (condition (var) (format t "ERROR: ~A~%" var)))
  (quit))
