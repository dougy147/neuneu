; NETWORK
; Define network objects and functions
; e.g. weights matrices, layers, cost function;..

(defun construct-network ()
  (assert (eq (nth 1 *output-dimensions*) (nth 0 (last *network-architecture*))) ()
	  "ERROR: Last provided layer has ~d neurons while output expects ~d values."
	  (nth 0 (last *network-architecture*)) (nth 1 *output-dimensions*))

  (setq *depth* (list-length *network-architecture*))

  (defun one-row-array ()
    (make-array (+ (list-length *network-architecture*) 1)))

  ; Lists of matrix struct linking pure arrays (see below)
  (setq *ws*     (one-row-array))
  (setq *bs*     (one-row-array))
  (setq *as*     (one-row-array))
  (setq *tmp-as* (one-row-array)) ; useful matrice

  ; Lists of pure arrays
  (setq *weights*     (one-row-array)) (setq *tmp-weights*     (one-row-array))
  (setq *biases*      (one-row-array)) (setq *tmp-biases*      (one-row-array))
  (setq *activations* (one-row-array)) (setq *tmp-activations* (one-row-array))

  (setq input-rows 1)
  (setq prev-rows (nth 0 *input-dimensions*))
  (setq prev-cols (nth 1 *input-dimensions*))

  ; Prepare matrices for input
  (setf (aref *weights*     0) NIL) (setf (aref *tmp-weights*     0) NIL)
  (setf (aref *biases*      0) NIL) (setf (aref *tmp-biases*      0) NIL)
  (setf (aref *activations* 0)     (make-array (* 1 prev-cols)))
  (setf (aref *tmp-activations* 0) (make-array (* 1 prev-cols)))

  (setf (aref *ws* 0) (make-mat :rows input-rows
			        :cols prev-cols
			        :vals (aref *weights* 0)))
  (setf (aref *bs* 0) (make-mat :rows input-rows
			        :cols prev-cols
			        :vals (aref *biases* 0)))
  (setf (aref *as* 0) (make-mat :rows input-rows
			        :cols prev-cols
			        :vals (aref *activations* 0)))
  (setf (aref *tmp-as* 0) (make-mat :rows input-rows
			        :cols prev-cols
			        :vals (aref *tmp-activations* 0)))

  ; Prepare matrices for layers
  (setq layer-index 1)
  (loop for nb-neurons in *network-architecture* do
	(setf (aref *weights*     layer-index) (make-array (* prev-cols nb-neurons)))
  	(setf (aref *biases*      layer-index) (make-array (* input-rows nb-neurons)))
  	(setf (aref *activations* layer-index) (make-array (* input-rows nb-neurons)))

	(setf (aref *tmp-weights*     layer-index) (make-array (* prev-cols nb-neurons)))
  	(setf (aref *tmp-biases*      layer-index) (make-array (* input-rows nb-neurons)))
  	(setf (aref *tmp-activations* layer-index) (make-array (* input-rows nb-neurons)))

        (setf (aref *ws* layer-index) (make-mat :rows prev-cols
						:cols nb-neurons
						:vals (aref *weights* layer-index)))
        (setf (aref *bs* layer-index) (make-mat :rows input-rows
						:cols nb-neurons
						:vals (aref *biases* layer-index)))
        (setf (aref *as* layer-index) (make-mat :rows input-rows
						:cols nb-neurons
						:vals (aref *activations* layer-index)))

        (setf (aref *tmp-as* layer-index) (make-mat :rows input-rows
						:cols nb-neurons
						:vals (aref *tmp-activations* layer-index)))
	(incf layer-index)
  	(setq prev-cols nb-neurons))
  (init-with-random-vals *ws*)
  (init-with-random-vals *bs*)
  (init-with-random-vals *as*)
  (init-with-random-vals *tmp-as*))

(defun init-with-random-vals (arr)
  (loop for i from 1 to (- (length arr) 1) do
	(loop for j from 0 to (- (mat-rows (aref arr i)) 1) do
	    (loop for k from 0 to (- (mat-cols (aref arr i)) 1) do
	      (mat-set-at (aref arr i) j k (random 1.0))))))

(defun load-input (&key rows cols vals)
  "Input needs dimensions and an ARRAY of values"
  (setq *input-dimensions* (list rows cols))   ; '(rows cols) of input
  (setq *input* (make-mat :rows rows
		          :cols cols
		          :vals vals))
  (setq *input-example-index* -1))

(defun load-output (&key rows cols vals)
  "Output needs dimensions and an ARRAY of values"
  (setq *output-dimensions* (list rows cols))   ; '(rows cols) of input
  (setq *output* (make-mat :rows rows
		           :cols cols
		           :vals vals)))

(defun engage-input-example (input) ; only loading arrays for now
  (setf (mat-vals (aref *as* 0)) input)
  (setf (aref *activations* 0) input))

(defun engage-expected-output () ; only loading arrays for now
  (setq output-data-index (* *input-example-index* (mat-cols *output*)))
  (setq *expected-output* (make-mat :rows 1
				    :cols (mat-cols *output*)
				    :vals (subseq (mat-vals *output*) output-data-index (+ output-data-index (mat-cols *output*))))))

(defun engage-input-next-example ()
  (setq *input-example-index* (mod (incf *input-example-index*) (mat-rows *input*)))
  (setq input-data-index (* *input-example-index* (mat-cols *input*)))
  (engage-input-example (subseq (mat-vals *input*) input-data-index (+ input-data-index (mat-cols *input*))))
  (engage-expected-output))

(defun sigmoid (val)
  (/ 1.0 (+ 1.0 (exp (- val)))))

(defun sigmoid-activations (as-index)
  (loop for r from 0 to (- (mat-rows (aref *as* as-index)) 1) do
    (loop for c from 0 to (- (mat-cols (aref *as* as-index)) 1) do
      (setq value (sigmoid (mat-get-at (aref *as* as-index) r c)))
      (mat-set-at (aref *as* as-index) r c value))))

(defun forward ()
  (loop for i from 1 to *depth* do
	(mat-dot-in (aref *tmp-as* i) (aref *as* (- i 1)) (aref *ws* i))
	(mat-add-in (aref *as* i)     (aref *tmp-as* i)   (aref *bs* i))
	(sigmoid-activations i)))

(defun get-cost ()
  (setq last-activation (aref *as* *depth*))
  (mat-cost last-activation *expected-output*))
  ;(setq ncost 0)
  ;(loop for r from 0 to (- (mat-rows last-activation) 1) do
  ;      (loop for c from 0 to (- (mat-cols *expected-output*) 1) do
  ;            (setq distance (- (mat-get-at last-activation r c) (mat-get-at *expected-output* r c)))
  ;            (setq ncost (+ ncost (* distance distance)))))
  ;(/ ncost (mat-rows *input*)))

(defun reduce-cost ()
  (setq *example-cost* (get-cost))
  (loop for i from 1 to *depth* do
    ; start with weights
    (loop for j from 0 to (- (length (aref *weights* i)) 1) do
	  (setq w-bak (aref (aref *weights* i) j))
	  (setf (aref (aref *weights* i) j) (+ w-bak *eps*))
	  (forward)
	  (setq c2 (get-cost))
	  (setq w-new (- w-bak (/ (* *rate* (- c2 *example-cost*)) *eps*)))
	  (setf (aref (aref *tmp-weights* i) j) w-new)
	  (setf (aref (aref *weights* i) j) w-bak))
    ; then with biases
    (loop for j from 0 to (- (length (aref *biases* i)) 1) do
	  (setq b-bak (aref (aref *biases* i) j))
	  (setf (aref (aref *biases* i) j) (+ b-bak *eps*))
	  (forward)
	  (setq c2 (get-cost))
	  (setq b-new (- b-bak (/ (* *rate* (- c2 *example-cost*)) *eps*)))
	  (setf (aref (aref *tmp-biases* i) j) b-new)
	  (setf (aref (aref *biases* i) j) b-bak))
    )
  ; set new matrices in place
  (loop for i from 1 to *depth* do
    (loop for j from 0 to (- (length (aref *weights* i)) 1) do
	(setf (aref (aref *weights* i) j) (aref (aref *tmp-weights* i) j)))
    (loop for j from 0 to (- (length (aref *biases* i)) 1) do
	(setf (aref (aref *biases* i) j) (aref (aref *tmp-biases* i) j))))
)

(defun learn (iterations)
  (loop for iter from 1 to iterations do
    (setq cost 0)
    (loop for _ from 1 to (mat-rows *input*) do
	(engage-input-next-example)
	(forward)
	(reduce-cost)
	(setq cost (+ cost *example-cost*))
    )
    (if (<= cost *stop*) (return) ())
    (format t "~d: ~f~%" iter cost)))

(defun check-results ()
  (setq *input-example-index* -1)
  (format t "------------------------------------~%")
  (loop for _ from 1 to (mat-rows *input*) do
	(engage-input-next-example)
	(forward)
	(format t "  input          : ~a~%" (mat-vals (aref *as* 0)))
	(format t "  expected output: ~a~%" (mat-vals *expected-output*))
	(format t "  network output : ~a~%" (mat-vals (aref *as* *depth*)))
	(format t "------------------------------------~%")))
