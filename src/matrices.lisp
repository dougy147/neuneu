; MATRICES.LISP
; Define matrices structures.
; Handle matrices operations.

; Matrices vals are arrays in this implementation.

(defstruct mat rows cols vals)

(defun mat-index-at (m r c)
  (+ (* r (mat-cols m)) c))

(defun mat-get-at (m r c)
  (aref (mat-vals m) (mat-index-at m r c)))

(defun mat-set-at (m r c val)
  (setf (aref (mat-vals m) (mat-index-at m r c)) val))

(defun mat-dot (m0 m1)
  "Returns m0 * m1 in a new matrix"
  (assert (eq (mat-cols m0) (mat-rows m1)) ()
	  "ERROR: Cannot multiply m0 and m1:~%m0: ~a~%m1: ~a" m0 m1)
  (setq m2 (make-mat :rows (mat-rows m0)
		     :cols (mat-cols m1)
	    	     :vals (make-array (* (mat-rows m0) (mat-cols m1)))))
  (loop for r from 0 to (- (mat-rows m0) 1) do
	(loop for c from 0 to (- (mat-cols m1) 1) do
	      (setq sum 0)
	      (loop for x from 0 to (- (mat-cols m0) 1) do
		    (setq mult (* (mat-get-at m0 r x) (mat-get-at m1 x c)))
		    (setq sum (+ sum mult)))
	      (mat-set-at m2 r c sum)))
  m2)

(defun mat-dot-in (mdest m0 m1)
  "Returns m0 * m1 in mdest"
  (assert (eq (mat-cols m0) (mat-rows m1)) ()
	  "ERROR: Cannot multiply m0 and m1:~%m0: ~a~%m1: ~a" m0 m1)
  (assert (and (eq (mat-rows mdest) (mat-rows m0)) (eq (mat-cols mdest) (mat-cols m1))) ()
	  "ERROR: Cannot store multiplication of m0 and m1 in mdest:~%m0: ~a~%m1: ~a~%mdest: ~a" m0 m1 mdest)
  (loop for r from 0 to (- (mat-rows m0) 1) do
	(loop for c from 0 to (- (mat-cols m1) 1) do
	      (setq sum 0)
	      (loop for x from 0 to (- (mat-cols m0) 1) do
		    (setq mult (* (mat-get-at m0 r x) (mat-get-at m1 x c)))
		    (setq sum (+ sum mult)))
	      (mat-set-at mdest r c sum))))

(defun mat-add (m0 m1)
  "Returns m0 + m1 in a new matrix"
  (assert (eq (mat-rows m0) (mat-rows m1)) ()
	  "ERROR: Cannot add m0 and m1. Rows mismatch:~%m0: ~a~%m1: ~a" m0 m1)
  (assert (eq (mat-cols m0) (mat-cols m1)) ()
	  "ERROR: Cannot add m0 and m1. Columns mismatch:~%m0: ~a~%m1: ~a" m0 m1)
  (setq m2 (make-mat :rows (mat-rows m1)
		     :cols (mat-cols m0)
		     :vals (make-array (* (mat-rows m0) (mat-cols m1)))))
  (loop for r from 0 to (- (mat-rows m0) 1) do
	(loop for c from 0 to (- (mat-cols m1) 1) do
	      (mat-set-at m2 r c (+ (mat-get-at m0 r c) (mat-get-at m1 r c)))))
  m2)

(defun mat-add-in (mdest m0 m1)
  "Returns m0 + m1 in a mdest"
  (assert (eq (mat-rows m0) (mat-rows m1)) ()
	  "ERROR: Cannot add m0 and m1. Rows mismatch:~%m0: ~a~%m1: ~a" m0 m1)
  (assert (eq (mat-cols m0) (mat-cols m1)) ()
	  "ERROR: Cannot add m0 and m1. Columns mismatch:~%m0: ~a~%m1: ~a" m0 m1)
  (assert (and (eq (mat-rows mdest) (mat-rows m0)) (eq (mat-cols mdest) (mat-cols m1))) ()
	  "ERROR: Cannot add m0 and m1 in mdest:~%m0: ~a~%m1: ~a~%mdest: ~a" m0 m1 mdest)
  (loop for r from 0 to (- (mat-rows m0) 1) do
	(loop for c from 0 to (- (mat-cols m1) 1) do
	      (mat-set-at mdest r c (+ (mat-get-at m0 r c) (mat-get-at m1 r c))))))

(defun mat-cost (m0 m1) ; used for cost / compute distance
  (assert (eq (mat-rows m0) (mat-rows m1)) ()
	  "ERROR: Cannot compute distance between m0 and m1. Rows mismatch:~%m0: ~a~%m1: ~a" m0 m1)
  (assert (eq (mat-cols m0) (mat-cols m1)) ()
	  "ERROR: Cannot compute distance between m0 and m1. Columns mismatch:~%m0: ~a~%m1: ~a" m0 m1)
  (setq sum 0)
  (loop for r from 0 to (- (mat-rows m0) 1) do
	(loop for c from 0 to (- (mat-cols m1) 1) do
	      (setq distance (- (mat-get-at m0 r c) (mat-get-at m1 r c)))
	      (setq sum (+ sum (* distance distance)))))
  (/ sum (mat-rows *input*)))
