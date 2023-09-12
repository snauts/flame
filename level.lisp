(defun tile (id &key (pr 0) (pl 0) (v 0) (h 0))
  (logior (ash pl 13) (ash pr 15) (ash v 12) (ash h 11) id))

(defun display-id (id)
  (if (null id)
      (format t "   .")
      (format t "~4,' d" (logand id #x7ff))))

(defun width (box)
  (length box))

(defun height (box)
  (reduce #'max (mapcar #'length box)))

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

(defun cut (list a b)
  (butlast (nthcdr a list) (max 0 (- (length list) b))))

(defun crop (x1 y1 x2 y2 box)
  (mapcar (lambda (column) (cut column y1 y2)) (cut box x1 x2)))

(defun multiply (box n)
  (let ((result nil))
    (dotimes (i n result)
      (setf result (append box result)))))

(defun index-pair (columns prev)
  (logior (or (length (first columns)) 0)
	  (ash (or (length prev) 0) 8)))

(defun serialize (box &optional prev flat)
  (labels ((add (x) (push x flat)))
    (add (index-pair box prev))
    (mapc #'add (first box))
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

(defun save-array (out name words)
  (format out "const u16 ~A[] = {~%" name)
  (save-words out words)
  (format out "};~%"))

(load "desert.lisp")

(defun save-level ()
  (with-open-file (out "level.inc" :if-exists :supersede :direction :output)
    (save-array out "desert_level" (desert-level))))

(defun save-and-quit ()
  (handler-case (save-level)
    (condition (var) (format t "ERROR: ~A~%" var)))
  (quit))
