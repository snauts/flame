(defparameter *notes*
  '((C  617) (Cs 653) (D  692)  (Ds 733)
    (E  777) (F  823) (Fs 872)  (G  924)
    (Gs 979) (A 1037) (As 1099) (B 1164) (X -1)))

(defparameter *johnny*
  '((2 (0 0 D))
    (2 (0 0 G)  (4 0 D) (5 0 G) (6 0 As))
    (2 (0 0 G))
    (2 (0 0 G))
    (4 (0 0 G))
    (2 (0 0 A))
    (4 (0 0 As) (4 0 D) (5 0 G) (6 0 As))
    (2 (0 0 A))
    (4 (0 0 As))
    (2 (0 0 G))
    (6 (0 0 F)  (4 0 D) (5 0 F) (6 0 As))
    (4                  (5 0 F))
    (2 (0 0 D))
    (6 (0 0 F)  (4 0 D) (5 0 F) (6 0 As))
    (4                  (5 0 F))
    (2 (0 0 D))
    (2 (0 0 G)  (4 0 D) (5 0 G))
    (2 (0 0 G))
    (2 (0 0 G))
    (4 (0 0 G))
    (2 (0 0 A))
    (4 (0 0 As) (4 0 D) (5 0 G))
    (2 (0 0 A))
    (4 (0 0 As))
    (2 (0 1 C))
    (6 (0 1 D)  (4 0 D) (5 0 F) (6 0 As))
    (4                  (5 0 F))
    (2 (0 0 As))
    (6 (0 1 D)  (4 0 D) (5 0 F) (6 0 As))
    (2                  (5 0 F))
    (2 (0 0 As))
    (2 (0 1 C))
    (4 (0 1 D)  (4 0 D) (5 0 F))
    (2 (0 1 D))
    (2 (0 1 D))
    (2 (0 1 C))
    (2 (0 0 As))
    (4 (0 1 C)  (4 0 C) (5 0 Ds) (6 0 G))
    (2 (0 1 C))
    (2 (0 1 C))
    (2 (0 0 As))
    (2 (0 0 A))
    (4 (0 0 As))
    (2 (0 0 As))
    (2 (0 0 As))
    (2 (0 0 A))
    (2 (0 0 G))
    (4 (0 0 A))
    (2 (0 0 A))
    (2 (0 0 A))
    (2 (0 0 As))
    (2 (0 1 C))
    (6 (0 1 D))
    (6 (0 1 C))
    (6 (0 0 As))
    (6 (0 0 A))
    (2 (0 0 G))
    (2 (0 0 G))
    (2 (0 0 G))
    (4 (0 0 G))
    (2 (0 0 F))
    (10(0 0 G))))

(defun save-bytes (out bytes)
  (let ((count 0))
    (dolist (b bytes)
      (format out "0x~2,'0X, " b)
      (when (>= (incf count) 8)
	(format out "~%")
	(setf count 0)))
    (when (/= count 0)
      (format out "~%"))))

(defun save-array (out name bytes)
  (format out "const byte ~A[] = {~%" name)
  (save-bytes out bytes)
  (format out "};~%"))

(defun bit-n (x)
  (ash 1 x))

(defun select-channels (chord)
  (reduce #'logior (mapcar #'bit-n (mapcar #'first (rest chord)))))

(defun lookup-note (note)
  (destructuring-bind (channel octave frequency) note
    (declare (ignore channel))
    (if (< frequency 0)
	(list (bit-n 7) 0)
	(list (logior (ash frequency -8) (ash octave 3))
	      (logand frequency #xff)))))

(defun is-inconsistent (chord)
  (let ((notes (rest chord)))
    (/= (length notes) (length (remove-duplicates notes :key #'first)))))

(defun sort-chord (chord)
  (when (is-inconsistent chord)
    (error "chord inconsistentcy~%~A" chord))
  (sort (copy-list chord) #'< :key #'first))

(defun save-score (score)
  (let ((result nil))
    (dolist (chord score)
      (push (select-channels chord) result)
      (dolist (note (sort-chord (rest chord)))
	(dolist (x (lookup-note note))
	  (push x result)))
      (push (first chord) result))
    (push 0 result)
    (reverse result)))

(defun new-note (new old-note)
  (cons new (copy-list (rest old-note))))

(defun find-note (channel chord)
  (assoc channel (rest chord)))

(defun attach (list item)
  (setf (rest (last list)) (list item)))

(defun scale-chord (chord amount)
  (setf (first chord) (* amount (first chord))))

(defun scale-tempo (score amount)
  (mapc (lambda (chord) (scale-chord chord amount)) score))

(defun adjust-note (score data adjust-fn)
  (dolist (chord score)
    (dolist (note (rest chord))
      (funcall adjust-fn note data))))

(defun adjust-note-octaves (note octaves)
  (setf (second note) (+ (second note) (elt octaves (first note)))))

(defun adjust-octaves (score octaves)
  (adjust-note score octaves #'adjust-note-octaves))

(defun get-frequency (sym frequencies)
  (or (second (assoc sym frequencies)) sym))

(defun adjust-note-frequency (note frequencies)
  (setf (third note) (get-frequency (third note) frequencies)))

(defun replace-frequencies (score frequencies)
  (adjust-note score frequencies #'adjust-note-frequency))

(defun key-off (num)
  (list num 0 -1))

(defun is-key-off (note)
  (member (third note) '(-1 X)))

(defun position-key-off (score period note)
  (let* ((chord (first score))
	 (duration (first chord)))
    (cond ((null score)
	   (error "key-off past end of score "))
	  ((= 0 period)
	   (attach chord note))
	  ((>= period duration)
	   (when (find-note (first note) (first (rest score)))
	     (error "channel ~A has same note sooner" (first note)))
	   (position-key-off (rest score) (- period duration) note))
	  ((< period duration)
	   (let ((tail (rest score)))
	     (setf (rest score) nil)
	     (setf (first chord) period)
	     (attach score (list (- duration period) note))
	     (unless (null tail) (setf (rest (last score)) tail)))))))

(defun channel-key-off (score num period)
  (unless (null score)
    (let ((note (find-note num (first score))))
      (when (and note (not (is-key-off note)))
	(position-key-off score period (key-off num))))
    (channel-key-off (rest score) num period)))

(defun duplicate-offset (score old new period)
  (unless (null score)
    (let ((note (find-note old (first score))))
      (when note (position-key-off score period (new-note new note))))
    (duplicate-offset (rest score) old new period)))

(defun duplicate-channel (score old new)
  (duplicate-offset score old new 0))

(defun johnny-score ()
  (let ((score (copy-tree *johnny*)))
    (duplicate-channel score 0 1)
    (duplicate-channel score 0 2)
    (adjust-octaves score '(1 2 0 x 0 0 0))
    (replace-frequencies score *notes*)
    (channel-key-off score 2 1)
    (scale-tempo score 5)
    score))

(defun save-music ()
  (with-open-file (out "music.inc" :if-exists :supersede :direction :output)
    (save-array out "johnny_score" (save-score (johnny-score)))))

(defun save-and-quit ()
  (handler-case (save-music)
    (condition (var) (format t "ERROR: ~A~%" var)))
  (quit))