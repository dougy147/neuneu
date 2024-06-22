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
  (setq *gradient-ws* (one-row-array)) ; useful matrice
  (setq *gradient-bs* (one-row-array)) ; useful matrice
  (setq *gradient-as* (one-row-array)) ; useful matrice

  ; Lists of pure arrays
  (setq *weights*     (one-row-array)) (setq *gradient-weights*     (one-row-array))
  (setq *biases*      (one-row-array)) (setq *gradient-biases*      (one-row-array))
  (setq *activations* (one-row-array)) (setq *gradient-activations* (one-row-array))

  (setq input-rows 1)
  (setq prev-rows (nth 0 *input-dimensions*))
  (setq prev-cols (nth 1 *input-dimensions*))

  ; Prepare matrices for input
  (setf (aref *weights*     0) (make-array (* prev-rows prev-cols)))
  (setf (aref *biases*      0) (make-array (* 1 prev-cols)))
  (setf (aref *activations* 0) (make-array (* 1 prev-cols)))

  (setf (aref *gradient-weights*     0) (make-array (* prev-rows prev-cols)))
  (setf (aref *gradient-biases*      0) (make-array (* 1 prev-cols)))
  (setf (aref *gradient-activations* 0) (make-array (* 1 prev-cols)))

  (setf (aref *ws* 0) (make-mat :rows prev-rows
			        :cols prev-cols
			        :vals (aref *weights* 0)))
  (setf (aref *bs* 0) (make-mat :rows input-rows
			        :cols prev-cols
			        :vals (aref *biases* 0)))
  (setf (aref *as* 0) (make-mat :rows input-rows
			        :cols prev-cols
			        :vals (aref *activations* 0)))
  (setf (aref *gradient-ws* 0) (make-mat :rows prev-rows
			        :cols prev-cols
			        :vals (aref *gradient-weights* 0)))
  (setf (aref *gradient-bs* 0) (make-mat :rows input-rows
			        :cols prev-cols
			        :vals (aref *gradient-biases* 0)))
  (setf (aref *gradient-as* 0) (make-mat :rows input-rows
			        :cols prev-cols
			        :vals (aref *gradient-activations* 0)))

  ; Prepare matrices for layers
  (setq layer-index 1)
  (loop for nb-neurons in *network-architecture* do
	(setf (aref *weights*     layer-index) (make-array (* prev-cols nb-neurons)))
  	(setf (aref *biases*      layer-index) (make-array (* input-rows nb-neurons)))
  	(setf (aref *activations* layer-index) (make-array (* input-rows nb-neurons)))

	(setf (aref *gradient-weights*     layer-index) (make-array (* prev-cols nb-neurons)))
  	(setf (aref *gradient-biases*      layer-index) (make-array (* input-rows nb-neurons)))
  	(setf (aref *gradient-activations* layer-index) (make-array (* input-rows nb-neurons)))

        (setf (aref *ws* layer-index) (make-mat :rows prev-cols
						:cols nb-neurons
						:vals (aref *weights* layer-index)))
        (setf (aref *bs* layer-index) (make-mat :rows input-rows
						:cols nb-neurons
						:vals (aref *biases* layer-index)))
        (setf (aref *as* layer-index) (make-mat :rows input-rows
						:cols nb-neurons
						:vals (aref *activations* layer-index)))

        (setf (aref *gradient-ws* layer-index) (make-mat :rows prev-cols
						:cols nb-neurons
						:vals (aref *gradient-weights* layer-index)))
        (setf (aref *gradient-bs* layer-index) (make-mat :rows input-rows
						:cols nb-neurons
						:vals (aref *gradient-biases* layer-index)))
        (setf (aref *gradient-as* layer-index) (make-mat :rows input-rows
						:cols nb-neurons
						:vals (aref *gradient-activations* layer-index)))
	(incf layer-index)
  	(setq prev-cols nb-neurons))

  (init-with-random-vals *ws*)
  (init-with-random-vals *bs*)
  (init-with-zero-vals   *as*)

  (init-with-zero-vals *gradient-ws*)
  (init-with-zero-vals *gradient-bs*)
  (init-with-zero-vals *gradient-as*))

(defun reset-gradient-layers ()
  "Set all gradient-layers values to zero"
  (init-with-zero-vals *gradient-ws*)
  (init-with-zero-vals *gradient-bs*)
  (init-with-zero-vals *gradient-as*))

(defun init-with-random-vals (arr)
  (loop for i from 0 to (- (length arr) 1) do
	(loop for j from 0 to (- (mat-rows (aref arr i)) 1) do
	    (loop for k from 0 to (- (mat-cols (aref arr i)) 1) do
	      (mat-set-at (aref arr i) j k (random 1.0))))))

(defun init-with-zero-vals (arr)
  (loop for i from 0 to (- (length arr) 1) do
	(loop for j from 0 to (- (mat-rows (aref arr i)) 1) do
	    (loop for k from 0 to (- (mat-cols (aref arr i)) 1) do
	      (mat-set-at (aref arr i) j k 0)))))

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
	(mat-dot-in (aref *as* i) (aref *as* (- i 1)) (aref *ws* i))
	(mat-add-in (aref *as* i) (aref *as* i)   (aref *bs* i))
	(sigmoid-activations i)))

(defun backprop ()
  (reset-gradient-layers)
  (loop for n from 0 to (- (mat-rows *input*) 1) do
	(engage-input-next-example)
	(init-with-zero-vals *gradient-as*)
	(forward)

	(loop for x from 0 to (- (mat-cols *output*) 1) do
	      (setq diff (- (mat-get-at (aref *as* *depth*) 0 x) (mat-get-at *output* n x)))
	      (mat-set-at (aref *gradient-as* *depth*) 0 x diff))

	(loop for l downfrom *depth* to 1 do
	      (loop for j from 0 to (- (mat-cols (aref *as* l)) 1) do
		    (setq a  (mat-get-at (aref *as* l)     0 j))
		    (setq da (mat-get-at (aref *gradient-as* l) 0 j))

		    (setq gradient-b  (aref *gradient-bs* l))
		    (setq prev-b (mat-get-at gradient-b 0 j))
		    (mat-set-at gradient-b 0 j (+ prev-b (* 2 (* da (* a (- 1 a))))))

		    (loop for k from 0 to (- (mat-cols (aref *as* (- l 1))) 1) do
			  (setq pa (mat-get-at (aref *as* (- l 1)) 0 k))
			  (setq w  (mat-get-at (aref *ws* l) k j))
			  (setq prev-w (mat-get-at (aref *gradient-ws* l) k j))
			  (setq prev-a (mat-get-at (aref *gradient-as* (- l 1)) 0 k))
			  (mat-set-at (aref *gradient-ws* l) k j (+ prev-w (* (* 2 (* da (* a (- 1 a)))) pa)))
			  (mat-set-at (aref *gradient-as* (- l 1)) 0 k (+ prev-a (* (* 2 (* da (* a (- 1 a)))) w)))))))

  (loop for l from 0 to *depth* do
	(loop for i from 0 to (- (mat-rows (aref *gradient-ws* l)) 1) do
	      (loop for j from 0 to (- (mat-cols (aref *gradient-ws* l)) 1) do
		    (setq old-w (mat-get-at (aref *ws* l) i j))
		    (mat-set-at (aref *ws* l) i j (- old-w (/ (mat-get-at (aref *gradient-ws* l) i j) (mat-rows *input*))))))
	(loop for i from 0 to (- (mat-rows (aref *gradient-bs* l)) 1) do
	      (loop for j from 0 to (- (mat-cols (aref *gradient-bs* l)) 1) do
		    (setq old-b (mat-get-at (aref *bs* l) i j))
		    (mat-set-at (aref *bs* l) i j (- old-b (/ (mat-get-at (aref *gradient-bs* l) i j) (mat-rows *input*))))))))

(defun get-cost ()
  (setq last-activation (aref *as* *depth*))
  (mat-cost last-activation *expected-output*))

(defun reduce-cost () ; naive cost reduction
  (setq *example-cost* (get-cost))
  (loop for i from 1 to *depth* do
    ; start with weights
    (loop for j from 0 to (- (length (aref *weights* i)) 1) do
	  (setq w-bak (aref (aref *weights* i) j))
	  (setf (aref (aref *weights* i) j) (+ w-bak *eps*))
	  (forward)
	  (setq c2 (get-cost))
	  (setq w-new (- w-bak (/ (* *rate* (- c2 *example-cost*)) *eps*)))
	  (setf (aref (aref *gradient-weights* i) j) w-new)
	  (setf (aref (aref *weights* i) j) w-bak))
    ; then with biases
    (loop for j from 0 to (- (length (aref *biases* i)) 1) do
	  (setq b-bak (aref (aref *biases* i) j))
	  (setf (aref (aref *biases* i) j) (+ b-bak *eps*))
	  (forward)
	  (setq c2 (get-cost))
	  (setq b-new (- b-bak (/ (* *rate* (- c2 *example-cost*)) *eps*)))
	  (setf (aref (aref *gradient-biases* i) j) b-new)
	  (setf (aref (aref *biases* i) j) b-bak))
    )
  ; set new matrices in place
  (loop for i from 1 to *depth* do
    (loop for j from 0 to (- (length (aref *weights* i)) 1) do
	(setf (aref (aref *weights* i) j) (aref (aref *gradient-weights* i) j)))
    (loop for j from 0 to (- (length (aref *biases* i)) 1) do
	(setf (aref (aref *biases* i) j) (aref (aref *gradient-biases* i) j)))))

(defun learn-naive (iterations)
  (setq *eps*  1e-2)
  (setq *rate* 1e-1)
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

(defun learn-backprop (iterations)
  (loop for iter from 1 to iterations do
    (backprop)
    (setq cost (get-cost))
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

(defun check-network-for-custom-input (custom-input-array)
  "Provide an input array and see what the network predicts"
  (engage-input-example custom-input-array)
  (forward)
  (print (mat-vals (aref *as* *depth*))))
