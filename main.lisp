(load "src/matrices.lisp")
(load "src/network.lisp")
(load "src/lexer.lisp")

(setf *random-state* (make-random-state t))

(defun main ()
  (setq *stop* 1e-3) ; stop iterating when cost is below this value

  (load-training-data-from-txt-file "./examples/false-belief.txt")
  (setq *network-architecture* '(3 3)) ; list of neurons by layer { 2 x 1 }

  (construct-network)

  ;(learn-naive 100) ; forward learning ("naive approach")
  ;(check-results)

  (learn-backprop 100000) ; backprop-base learning
  (check-results)

  ;(check-network-for-custom-input #(1 1 0 0 0 1))

  )

(main)
