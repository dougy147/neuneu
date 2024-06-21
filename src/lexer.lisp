; LEXER.LISP
; Given an input file construct a
; readable input for the network.

; INFO: Files' data are parsed as integers
; INFO: Delimitors in files are "|"

(defun read-file-line-by-line (filepath)
  (with-open-file (content filepath)
    (loop for line = (read-line content nil)
	  while line collect line)))

(defun split-content (string delimitor)
  (loop for i = 0 then (1+ j)
	as j = (position delimitor string :start i)
	collect (subseq string i j)
	while j))

(defun strip-spaces (elems)
    (remove-if (lambda (x) (string= x "")) (split-content elems #\ )))

(defun count-before-sep (content sep)
  (setq c 0)
  (loop for s in (strip-spaces (nth 0 content)) do
	(if (string= sep s) (return c) ())
	(incf c))
  c)

(defun make-input-array (rows cols content)
  (setq input-array (make-array (* rows cols)))
  (setq index 0)
  (loop for line in content do
	(loop for i from 0 to (- cols 1) do
	      (setf (aref input-array index) (parse-integer (nth i (strip-spaces line))))
	      (incf index)))
  input-array)

(defun make-output-array (rows cols input-cols content)
  (setq output-array (make-array (* rows cols)))
  (setq index 0)
  (loop for line in content do
	(loop for i from (+ 1 input-cols) to (+ 1 input-cols (- cols 1)) do
	      (setf (aref output-array index) (parse-integer (nth i (strip-spaces line))))
	      (incf index)))
  output-array)

(defun verify-balance (input-cols output-cols content filename)
  (setq line-index 0)
  (loop for line in content do
	(assert (string= #\| (nth input-cols (strip-spaces line))) ()
		"ERROR: Unbalanced input/output training data in \"~a\" (line ~d)" filename line-index)
	(assert (eq (length (strip-spaces line)) (+ input-cols output-cols 1)) ()
		"ERROR: Unbalanced input/output training data in \"~a\" (line ~d)" filename line-index)
	(incf line-index)
	)
  )

(defun load-training-data-from-file (filename)
  (setq content (read-file-line-by-line filename))

  (setq input-dimension-rows (length content))
  (setq input-dimension-cols (count-before-sep content #\|))
  (setq input-vals (make-input-array input-dimension-rows
				    input-dimension-cols
				    content))

  (setq output-dimension-rows (length content))
  (setq output-dimension-cols (- (length (strip-spaces (nth 0 content))) input-dimension-cols 1))
  (setq output-vals (make-output-array output-dimension-rows
				      output-dimension-cols
				      input-dimension-cols
				      content))

  (verify-balance input-dimension-cols output-dimension-cols content filename)

  (load-input  :rows input-dimension-rows
	       :cols input-dimension-cols
	       :vals input-vals)
  (load-output :rows output-dimension-rows
	       :cols output-dimension-cols
	       :vals output-vals))
