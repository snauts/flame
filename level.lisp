(defun is-negative (x)
  (< x 0))

(defun tile (palette id)
  (logior (ash palette 13) id))

(defun display-id (id)
  (if (is-negative id)
      (format t "   .")
      (format t "~4,' d" id)))

(defun display (rectangle)
  (dolist (row rectangle)
    (mapc #'display-id row)
    (format t "~%")))

(defun integers (n &optional (i 0))
  (unless (= i n) (cons i (integers n (1+ i)))))

(defun make-rectangle (x y tile)
  (let ((result nil))
    (dotimes (i y (reverse result))
      (labels ((numbers (n) (+ tile (* n y) i)))
	(push (mapcar #'numbers (integers x)) result)))))

(defun crop (x y w h rectangle)
  (labels ((crop-row (row) (subseq row x (+ x w))))
    (mapcar #'crop-row (subseq rectangle y (+ y h)))))

(defun multiply-row (row n &optional result)
  (if (<= n 0) result (multiply-row row (1- n) (append result row))))

(defun multiply-rectange (rectangle n)
  (mapcar (lambda (row) (multiply-row row n)) rectangle))

(defun regular-ground ()
  (crop 0 4 8 4 (make-rectangle 8 8 (tile 1 97))))

(defun save-heads (&rest rest)
  (let ((save (remove-if #'is-negative rest)))
    (cons (length save) save)))

(defun index-pair (columns prev)
  (logior (or (first (first columns)) 0)
	  (ash (or (first prev) 0) 8)))

(defun reverse-tiles (columns)
  (reverse (rest (first columns))))

(defun flatten-columns (columns &optional prev flat)
  (labels ((add-to-flat (x) (push x flat)))
    (add-to-flat (index-pair columns prev))
    (mapc #'add-to-flat (reverse-tiles columns))
    (if (null columns)
	(reverse flat)
	(flatten-columns (rest columns) (first columns) flat))))

(defun serialize-level (rectangle)
  (flatten-columns (apply #'mapcar (cons #'save-heads rectangle))))

(defun desert-level ()
  (serialize-level (multiply-rectange (regular-ground) 16)))

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

(defun save-level ()
  (with-open-file (out "level.inc" :if-exists :supersede :direction :output)
    (save-array out "desert_level" (desert-level))))

(defun save-and-quit ()
  (handler-case (save-level)
    (condition (var) (format t "ERROR: ~A~%" var)))
  (quit))
